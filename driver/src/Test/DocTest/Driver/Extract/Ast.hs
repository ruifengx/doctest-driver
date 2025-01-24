-- | Description: DocTest abstract syntax trees.
-- Copyright: Copyright 2025, Ruifeng Xie
-- License: AGPL-3.0-or-later
-- Maintainer: Ruifeng Xie <ruifengx@outlook.com>
--
-- DocTests syntax trees and extraction from source code parsed using the GHC API.
module Test.DocTest.Driver.Extract.Ast
  -- * Location Information
  ( Loc
  -- * Entities
  , EntityKind (..)
  , Entity (..)
  , declEntity
  , nameEntity
  -- * DocTests
  , Module (..)
  , extractModule
  , DocTests (..)
  , DocLine (..)
  , spanDocLine
  , ExampleLine (..)
  , CapturedContent (..)
  , CaptureMethod (..)
  , IOHook (..)
  , HookFlavour (..)
  ) where

import Test.DocTest.Driver.Utils

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Exception (assert)
import Data.Bifunctor (second)
import Data.Char (isAlphaNum, isLower, isSpace)
import Data.Either (partitionEithers)
import Data.Foldable (foldMap')
import Data.Function (on, (&))
import Data.Functor (void, ($>), (<&>))
import Data.Functor.Compose (Compose (..))
import Data.Generics (Data, Typeable, everything, mkQ)
import Data.List qualified as List (intercalate, isPrefixOf, sortBy, sortOn, stripPrefix)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Monoid (First (..))
import Data.Semigroup (Min (..))
import GHC.Generics (Generic, Generically (..))
import Text.ParserCombinators.ReadP qualified as R

import GHC (GhcPs, ParsedModule (..), getLoc, getLocA, ms_hspp_file, ms_mod_name, pattern L, unLoc)
import GHC.Data.FastString (FastString, fsLit, mkFastString, unpackFS)
import GHC.Hs qualified as GHC
import GHC.HsToCore.Docs qualified as GHC (collectDocs)
import GHC.Types.Name.Occurrence (occNameFS)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc qualified as GHC

-- | Best-effort source location.
type Loc = Either FastString GHC.RealSrcLoc

noLoc :: Loc
noLoc = Left (fsLit "<no location info>")

toLoc :: GHC.SrcLoc -> Loc
toLoc (GHC.RealSrcLoc loc _) = Right loc
toLoc (GHC.UnhelpfulLoc msg) = Left msg

advanceLoc :: String -> Loc -> Loc
advanceLoc = fmap . flip (foldr (flip GHC.advanceSrcLoc))

advanceLocBy :: Int -> Loc -> Loc
advanceLocBy n = fmap (\loc -> GHC.mkRealSrcLoc (GHC.srcLocFile loc) (GHC.srcLocLine loc) (GHC.srcLocCol loc + n))

-- | Kind of declared entities.
data EntityKind
  = DataFamily        -- ^ @data family@.
  | TypeFamily        -- ^ @type family@, open or closed.
  | NewType           -- ^ @newtype@.
  | DataType          -- ^ @data@.
  | TypeSynonym       -- ^ @type@, type/kind/constraint synonym.
  | Class             -- ^ @class@, type classes.
  | Binding           -- ^ Top-level variable or function binding.
  | Signature         -- ^ Top-level variable or function signature.
  | PatternSynonym    -- ^ Pattern synonym definition.
  | PatternSignature  -- ^ Pattern synonym signature.
  | ForeignImport     -- ^ @foreign import@ declarations.
  | ForeignExport     -- ^ @foreign export@ declarations.
  | NamedDoc          -- ^ Haddock markup, named chunk (@$name@).
  deriving stock (Show, Eq, Ord)

-- | Declared entity information.
data Entity = Entity
  { kind :: !EntityKind -- ^ Entity kind.
  , name :: !FastString -- ^ Entity name.
  } deriving stock Eq

instance Show Entity where
  show Entity{ name = unpackFS -> name, kind } = case kind of
    DataFamily       -> "data family " <> name
    TypeFamily       -> "type family " <> name
    NewType          -> "newtype " <> name
    DataType         -> "data " <> name
    TypeSynonym      -> "type " <> name
    Class            -> "class " <> name
    Binding          -> name
    Signature        -> name
    PatternSynonym   -> "pattern " <> name
    PatternSignature -> "pattern " <> name
    ForeignImport    -> name
    ForeignExport    -> name
    NamedDoc         -> name

lIdString :: GHC.LIdP GhcPs -> FastString
lIdString = idString . unLoc

idString :: GHC.IdP GhcPs -> FastString
idString = occNameFS . rdrNameOcc

-- | Extract an 'Entity' for the declaration.
--
-- This is used to determine which declaration a Haddock document should attach to.
--
-- * @'Just' e@ if at least one entity is found, and @e@ is the first entity.
-- * 'Nothing' if there is no entity the document could attach to.
declEntity :: GHC.HsDecl GhcPs -> Maybe Entity
declEntity (GHC.TyClD _ decl) = case decl of
  GHC.FamDecl{}   -> Just Entity{ kind = familyKind decl.tcdFam.fdInfo, name = lIdString (GHC.fdLName decl.tcdFam) }
  GHC.SynDecl{}   -> Just Entity{ kind = TypeSynonym, name = lIdString decl.tcdLName }
  GHC.DataDecl{}  -> Just Entity{ kind = dataKind decl.tcdDataDefn.dd_cons, name = lIdString decl.tcdLName }
  GHC.ClassDecl{} -> Just Entity{ kind = Class, name = lIdString decl.tcdLName }
  where familyKind GHC.DataFamily = DataFamily
        familyKind _              = TypeFamily
        dataKind GHC.NewTypeCon{} = NewType
        dataKind _                = DataType
declEntity (GHC.ValD _ bind) = case bind of
  GHC.FunBind{ fun_id }  -> Just Entity{ kind = Binding, name = lIdString fun_id }
  GHC.VarBind{ var_id }  -> Just Entity{ kind = Binding, name = idString var_id }
  GHC.PatBind{ pat_lhs } -> gFirst pat_lhs <&> \name -> Entity{ kind = Binding, name }
  GHC.PatSynBind _ pat   -> Just Entity{ kind = PatternSynonym, name = lIdString pat.psb_id }
declEntity (GHC.SigD _ sig) = case sig of
  GHC.TypeSig   _ names _ -> Just Entity{ kind = Signature, name = lIdString (head names) }
  GHC.PatSynSig _ names _ -> Just Entity{ kind = PatternSignature, name = lIdString (head names) }
  _                       -> Nothing
declEntity (GHC.ForD _ decl) = case decl of
  GHC.ForeignImport{ fd_name } -> Just Entity{ kind = ForeignImport, name = lIdString fd_name }
  GHC.ForeignExport{ fd_name } -> Just Entity{ kind = ForeignExport, name = lIdString fd_name }
declEntity _ = Nothing

gFirst :: (Data a, Typeable b) => a -> Maybe b
gFirst = getFirst . everything (<>) (mkQ (First Nothing) First)

-- | Create a 'NamedDoc' entity representing a named doc comment.
nameEntity :: String -> Entity
nameEntity name = Entity{ kind = NamedDoc, name = mkFastString name }

-- | Extracted DocTest items.
data DocTests
  = Group Entity Loc [DocTests]         -- ^ Group, translated to @describe@.
  | TestProperty (NonEmpty DocLine)     -- ^ Property, to be tested with QuickCheck.
  | TestExample (NonEmpty ExampleLine)  -- ^ Example, compared with expected output.
  | TestMultiline (NonEmpty DocLine)    -- ^ Multiline tests.
  | Capture CapturedContent [DocTests]  -- ^ Captured string literal.
  | TestHook IOHook [DocTests]          -- ^ 'IO' hook: @before@, @beforeAll@, @after@, @afterAll@.
  | Warning Loc String                  -- ^ Problems detected while extracting the DocTests.
  deriving stock (Show, Eq)

-- | A single line from the DocTests, rendered with @{-# LINE ... #-}@ and @{-# COLUMN ... #-}@.
data DocLine = DocLine
  { location :: Loc     -- ^ Original source location.
  , textLine :: String  -- ^ Content of the line.
  } deriving stock (Show, Eq)

emptyLine :: DocLine
emptyLine = DocLine{ location = noLoc, textLine = "" }

replaceLeadingTabs :: DocLine -> DocLine
replaceLeadingTabs l = l{ textLine = replicate (restCol - startCol) ' ' ++ rest }
  where (leading, rest) = span isSpace l.textLine
        startCol = either (const 1) GHC.srcLocCol l.location
        restCol = foldr advanceCol startCol leading

advanceCol :: Char -> Int -> Int
advanceCol '\n' _ = error "no newline allowed in DocLine"
advanceCol '\t' n = ((n - 1) `div` 8 + 1) * 8 + 1
advanceCol _    n = n + 1

unindentLines :: Traversable t => t DocLine -> t DocLine
unindentLines (fmap replaceLeadingTabs -> theLines) = fmap dropSpace theLines
  where -- since all-white lines are excluded, we must not use 'minimum'
        -- because 'minimum' diverges for empty sequences
        level = maybe 0 getMin $ foldMap' (Just . Min) (Compose (fmap getLevel theLines))
        -- only consider non-white lines for unindent
        getLevel l = if null rest then Nothing else Just (length white)
          where (white, rest) = span isSpace l.textLine
        dropSpace l = DocLine (advanceLocBy level l.location) (drop level l.textLine)

stripPrefix :: String -> DocLine -> Maybe DocLine
stripPrefix p l = DocLine (advanceLoc p l.location) <$> List.stripPrefix p l.textLine

-- | 'span', but also handles the @location@ of 'DocLine's.
spanDocLine :: (Char -> Bool) -> DocLine -> (String, DocLine)
spanDocLine p l = (left, DocLine (advanceLoc left l.location) rest)
  where (left, rest) = span p l.textLine

-- NOTE: we unindent the lines by only removing extra leading /spaces/.
-- this means other whitespace characters like tabs are not considered at all.
trimLeft :: DocLine -> DocLine
trimLeft = snd . spanDocLine (== ' ')

trimPrefix :: String -> DocLine -> DocLine
trimPrefix p = trimLeft . fromJust . stripPrefix p . trimLeft

-- | A single line of example, accompanied by several output lines.
data ExampleLine = ExampleLine
  { programLine    :: DocLine   -- ^ Example line (code).
  , expectedOutput :: [DocLine] -- ^ Output text lines.
  } deriving stock (Show, Eq)

-- | Captured text content, made available as a variable.
data CapturedContent = Captured
  { variableName  :: String           -- ^ Name of the variable to bring into scope.
  , captureMethod :: CaptureMethod    -- ^ How the text content is captured.
  , textContent   :: NonEmpty String  -- ^ Text content to be captured.
  } deriving stock (Show, Eq)

-- | Different ways of capturing text contents.
data CaptureMethod
  = String            -- ^ t'String' variable.
  | TextStrict        -- ^ Strict @Text@ variable (from @Data.Text@).
  | TextLazy          -- ^ Lazy @Text@ variable (from @Data.Text.Lazy@).
  | ByteStringStrict  -- ^ Strict @ByteString@ variable (from @Data.ByteString@).
  | ByteStringLazy    -- ^ Lazy @ByteString@ variable (from @Data.ByteString.Lazy@).
  | ShortByteString   -- ^ @ShortByteString@ variable (from @Data.ByteString.Short@).
  | TempFile          -- ^ Save as a temporary file, provide as a 'FilePath' variable.
  deriving stock Eq

instance Show CaptureMethod where
  show String           = "String"
  show TextStrict       = "Strict.Text"
  show TextLazy         = "Lazy.Text"
  show ByteStringStrict = "Strict.ByteString"
  show ByteStringLazy   = "Lazy.ByteString"
  show ShortByteString  = "ShortByteString"
  show TempFile         = "FilePath"

-- | 'IO' actions to be performed throughout the test lifetime.
data IOHook = IOHook
  { flavour   :: HookFlavour      -- ^ Occasions when the 'IO' action is expected to run.
  , variables :: [String]         -- ^ List of variable names brought into scope by this hook.
  , setupCode :: NonEmpty DocLine -- ^ The code for the action.
  } deriving stock (Show, Eq)

-- | Occasions to run an 'IOHook'.
data HookFlavour
  = Before    -- ^ Once before each test case (result is freshly generated).
  | BeforeAll -- ^ Once before all test cases (result is shared).
  | After     -- ^ Once after each test case.
  | AfterAll  -- ^ Once after all test cases.
  deriving stock Eq

instance Show HookFlavour where
  show Before    = "before"
  show BeforeAll = "beforeAll"
  show After     = "after"
  show AfterAll  = "afterAll"

data SetupCode = SetupCode
  { setupImport :: [(String, NonEmpty DocLine)]
  , setupTop    :: [(String, NonEmpty DocLine)]
  }
  deriving stock (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically SetupCode

-- | Extracted module.
data Module = Module
  { filePath   :: FilePath    -- ^ Source file path to the original module.
  , modulePath :: [String]    -- ^ Module name path, separated by dots (@.@).
  , importList :: [DocLine]   -- ^ List of import statements to be put at the top.
  , topSetup   :: [DocLine]   -- ^ Top-level setup code, e.g., data type definitions.
  , testCases  :: [DocTests]  -- ^ Extracted test cases.
  } deriving stock Show

-- | Extract a 'Module' from a GHC 'ParsedModule'.
--
-- === Implementation Note
--
-- The documentation in a 'ParsedModule' is first extracted into a doc-tree (via 'GHC.collectDocs').
-- By design, doctest instructions can appear as group names (Haddock markup @\$group-name@) and as
-- comments in verbatim code block (Haddock markup @\@...\@@). Inside this doc-tree, we inspect
-- these places, recognise the instructions, and apply the instructions. Some instructions modify
-- their parent scope ('Capture's and 'IOHook's bring new names in scope), so we float them above.
extractModule :: ParsedModule -> Module
extractModule m
  = unLoc m.pm_parsed_source
  & GHC.hsmodDecls
  -- TODO: export list
  & List.sortBy (GHC.leftmost_smallest `on` getLocA)
  & GHC.collectDocs
  -- GHC doc-tree: [decl * [doc]]
  & concatMap (uncurry toDocTree)
  -- abstract doc-tree
  & map parseInstructions
  & mapTreeList (either (uncurry docWithInstruction) docNoInstruction)
  -- instructions extracted & properly attached
  & partitionTreeList (either (uncurry handleInstruction) pure)
  -- instructions applied
  & second (resolveScopes . map finaliseDocTree)
  -- doc-tree => doctest
  -- capture and hook floated above its parent scope
  & uncurry (collectModule m)
  -- module information collected

collectModule :: ParsedModule -> SetupCode -> [DocTests] -> Module
collectModule m setup testCases = Module{ filePath, modulePath, importList, topSetup, testCases }
  where filePath = m.pm_mod_summary.ms_hspp_file
        modulePath = takeModulePath (GHC.ms_mod_name m.pm_mod_summary)
        takeModulePath (GHC.ModuleName path) = splitBy '.' (unpackFS path)
        mergeGroups = List.intercalate [emptyLine] . map (NonEmpty.toList . snd) . List.sortOn fst
        importList = mergeGroups setup.setupImport
        topSetup = mergeGroups setup.setupTop

type Doc = GHC.HsDoc GhcPs
type LDoc = GHC.LHsDoc GhcPs

data DocTree a
  = DocEntry a
  | DocGroup Entity Loc [DocTree a]
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Applicative DocTree where
  pure = DocEntry
  DocEntry f      <*> r = fmap f r
  DocGroup e l xs <*> r = DocGroup e l (map (<*> r) xs)

instance Monad DocTree where
  DocEntry x      >>= k = k x
  DocGroup e l xs >>= k = DocGroup e l (map (>>= k) xs)

mapTreeList :: (a -> [b]) -> [DocTree a] -> [DocTree b]
mapTreeList f = concatMap go
  where go (DocEntry x)      = map DocEntry (f x)
        go (DocGroup e l xs) = [DocGroup e l (mapTreeList f xs)]

partitionTreeList :: Monoid b => (a -> Either b c) -> [DocTree a] -> (b, [DocTree c])
partitionTreeList f = second catMaybes . traverse go
  where go (DocEntry x)      = either (, Nothing) ((mempty, ) . Just . DocEntry) (f x)
        go (DocGroup e l xs) = mkGroup e l <$> partitionTreeList f xs
        mkGroup e l xs = if null xs then Nothing else Just (DocGroup e l xs)

toDocTree :: GHC.LHsDecl GhcPs -> [Doc] -> [DocTree (Either (String, LDoc) Doc)]
toDocTree (L _ (GHC.DocD _ docD)) docs = assert (null docs) case docD of
  GHC.DocCommentNamed name doc -> [DocEntry (Left (name, doc))]
  GHC.DocCommentNext doc       -> [DocEntry (Right (unLoc doc))]
  GHC.DocCommentPrev doc       -> [DocEntry (Right (unLoc doc))]
  GHC.DocGroup _ doc           -> [DocEntry (Right (unLoc doc))]
toDocTree decl docs = case declEntity (unLoc decl) of
  Just entity -> [DocGroup entity loc (map (DocEntry . Right) allDocs)]
  Nothing     -> map (DocEntry . Right) allDocs
  where loc = toLoc (GHC.srcSpanStart (getLocA decl))
        allDocs = everything (++) (mkQ [] pure) decl <> docs

finaliseDocTree :: DocTree DocTests -> DocTests
finaliseDocTree (DocEntry x)      = x
finaliseDocTree (DocGroup e l xs) = Group e l (map finaliseDocTree xs)

resolveScopes :: [DocTests] -> [DocTests]
resolveScopes ts = foldr ($) tests scopes
  where (scopes, tests) = partitionEithers (map resolveScope ts)

resolveScope :: DocTests -> Either ([DocTests] -> [DocTests]) DocTests
resolveScope (Group name loc tests)  = Right (Group name loc (resolveScopes tests))
resolveScope (TestHook hook tests)   = assert (null tests) (Left (pure . TestHook hook))
resolveScope (Capture content tests) = assert (null tests) (Left (pure . Capture content))
resolveScope t@(TestExample _)       = Right t
resolveScope t@(TestProperty _)      = Right t
resolveScope t@(TestMultiline _)     = Right t
resolveScope t@(Warning _ _)         = Right t

data Instruction
  = SetupImport String
  | SetupOther String
  | SetupAuto String
  | UseHook HookFlavour [String]
  | IsExample
  | IsProperty
  | CaptureText String CaptureMethod
  deriving stock Eq

mkParen :: String -> String
mkParen s = if null s then s else "(" ++ s ++ ")"

instance Show Instruction where
  show (SetupImport name)       = "setup-import" <> mkParen name
  show (SetupOther name)        = "setup-top" <> mkParen name
  show (SetupAuto name)         = "setup" <> mkParen name
  show (UseHook flavour vars)   = show flavour <> mkParen (List.intercalate ", " vars)
  show IsExample                = "test"
  show IsProperty               = "property"
  show (CaptureText var method) = "capture(" <> var <> " :: " <> show method <> ")"

lexeme :: R.ReadP a -> R.ReadP a
lexeme p = p <* R.skipSpaces

paren :: R.ReadP a -> R.ReadP a
paren p = lexeme (R.char '(') *> p <* lexeme (R.char ')')

text :: String -> R.ReadP ()
text = void . lexeme . R.string

anything :: R.ReadP String
anything = fst <$> R.gather (R.skipMany paired)
  where normal = R.munch1 (`notElem` "()")
        paired = normal <|> R.char '(' *> paired <* R.char ')'

ident :: R.ReadP String
ident = (:)
  <$> R.satisfy (\c -> isLower c || c == '_')
  <*> R.munch (\c -> isAlphaNum c || c == '_' || c == '\'')

pInstruction :: R.ReadP Instruction
pInstruction
  =   pSetup <*> R.option "" (paren anything)
  <|> pHook <*> R.option [] (paren (ident `R.sepBy` text ","))
  <|> text "test" $> IsExample
  <|> text "property" $> IsProperty
  <|> text "capture" *> paren (CaptureText <$> lexeme ident <* text "::" <*> lexeme pCaptureMethod)

pSetup :: R.ReadP (String -> Instruction)
pSetup
  =   SetupImport <$ text "setup-import"
  <|> SetupOther <$ text "setup-top"
  <|> SetupAuto <$ text "setup"

pHook :: R.ReadP ([String] -> Instruction)
pHook
  =   UseHook Before <$ text "before"
  <|> UseHook BeforeAll <$ text "beforeAll"
  <|> UseHook After <$ text "after"
  <|> UseHook AfterAll <$ text "afterAll"

pCaptureMethod :: R.ReadP CaptureMethod
pCaptureMethod
  =   String <$ R.string "String"
  <|> TextStrict <$ R.optional (R.string "Strict.") <* R.string "Text"
  <|> TextLazy <$ R.string "Lazy.Text"
  <|> ByteStringStrict <$ R.optional (R.string "Strict.") <* R.string "ByteString"
  <|> ByteStringLazy <$ R.string "Lazy.ByteString"
  <|> ShortByteString <$ R.string "ShortByteString"
  <|> TempFile <$ R.string "FilePath"

pCommentHead :: R.ReadP ()
pCommentHead = text "--" *> text "doctest:"

pInstructionComment :: R.ReadP Instruction
pInstructionComment = pCommentHead *> R.option IsExample pInstruction

parseInstructions :: DocTree (Either (String, LDoc) Doc) -> DocTree (Either (Instruction, Doc) Doc)
parseInstructions t = t >>= either (uncurry nameToInstruction) (DocEntry . Right)

nameToInstruction :: String -> LDoc -> DocTree (Either (Instruction, Doc) Doc)
nameToInstruction name doc = case R.readP_to_S (pInstruction <* R.skipSpaces <* R.eof) name of
  []           -> DocGroup (nameEntity name) loc [DocEntry (Right (unLoc doc))]
  [(instr, _)] -> DocEntry (Left (instr, unLoc doc))
  _ : _ : _    -> error "impossible: no ambiguity allowed in instruction syntax"
  where loc = toLoc (GHC.srcSpanStart (getLoc doc))

docLines :: GHC.HsDocString -> [DocLine]
docLines (GHC.MultiLineDocString _ lcs) = map docChunkLine (NonEmpty.toList lcs)
docLines (GHC.NestedDocString _ lc)     = docChunkLines lc
docLines (GHC.GeneratedDocString c)     = docChunkLines (L GHC.generatedSrcSpan c)

docChunkLine :: GHC.LHsDocStringChunk -> DocLine
docChunkLine (L l docText) = DocLine (toLoc (GHC.srcSpanStart l)) (GHC.unpackHDSC docText)

docChunkLines :: GHC.LHsDocStringChunk -> [DocLine]
docChunkLines (L l docText) = zipWith DocLine locs (dosLines (GHC.unpackHDSC docText))
  where locs = iterate (advanceLoc "\n") (toLoc (GHC.srcSpanStart l))

docNoInstruction :: Doc -> [Either (Instruction, [DocLine]) DocTests]
docNoInstruction = mapMaybe docGroupNormal . groupDoc

docWithInstruction :: Instruction -> Doc -> [Either (Instruction, [DocLine]) DocTests]
docWithInstruction instr = applyInstruction instr . groupDoc

groupDoc :: Doc -> [DocGroup]
groupDoc
  = mapMaybe (uncurry toDocGroup)
  . groupByKey (\l -> docLineType l.textLine) assocLineType
  . docLines . GHC.hsDocString

docGroupNormal :: DocGroup -> Maybe (Either (Instruction, [DocLine]) DocTests)
docGroupNormal (GExample ex)           = Just (Right (TestExample ex))
docGroupNormal (GProperty prop)        = Just (Right (TestProperty (NonEmpty.singleton prop)))
docGroupNormal (GVerbatim (c :| rest)) = Left . (, rest) <$> parseInstructionComment c.textLine

applyInstruction :: Instruction -> [DocGroup] -> [Either (Instruction, [DocLine]) DocTests]
applyInstruction instr g = Left (instr, code) : map Right warnings
  where (warnings, code) = foldMap plainCode g

plainCode :: DocGroup -> ([DocTests], [DocLine])
plainCode (GExample ls) = traverse go (NonEmpty.toList ls)
  where go l = (maybe [] (pure . warn) (nonEmpty l.expectedOutput), l.programLine)
        warn (l :| _) = Warning l.location "unexpected output text, expecting plain code"
plainCode (GProperty l) = ([Warning l.location "unexpected property, expecting plain code"], [])
plainCode (GVerbatim ls@(l :| _)) = go (parseInstructionComment l.textLine)
  where go = maybe ([], NonEmpty.toList ls) (const ([warning], []))
        warning = Warning l.location "unexpected instruction, expecting plain code"

parseInstructionComment :: String -> Maybe Instruction
parseInstructionComment s = case R.readP_to_S p s of
  []           -> Nothing
  [(instr, _)] -> Just instr
  _ : _ : _    -> error "impossible: no ambiguity allowed in instruction syntax"
  where p = pInstructionComment <* R.skipSpaces <* R.eof

data DocGroup
  = GProperty DocLine
  | GExample (NonEmpty ExampleLine)
  | GVerbatim (NonEmpty DocLine)
  deriving stock (Show, Eq)

toDocGroup :: LineType -> NonEmpty (LineType, DocLine) -> Maybe DocGroup
-- properties are one in each group
toDocGroup Property ((_, p) :| r) = assert (null r) (Just (GProperty (trimProperty p)))
-- examples are optionally followed by expected output
toDocGroup Example exampleLines = Just (collectExample exampleLines)
  where mkExampleLine (x :| rest) =
          assert (fst x == Example)
          assert (all ((== Other) . fst) rest)
          ExampleLine{ programLine, expectedOutput }
          where programLine = trimExample (snd x)
                expectedOutput = unindentLines (map snd rest)
        -- one example optionally followed by several responses
        assocExampleLine l r = l == Example && r == Other
        groupExamples = NonEmpty.groupBy1 (assocExampleLine `on` fst)
        collectExample = GExample . fmap mkExampleLine . groupExamples
-- verbatim code are preserved for future processing
toDocGroup Verbatim verbatimCode = Just (GVerbatim verbatimLines)
  where verbatimLines = unindentLines (fmap (trimVerbatim . snd) verbatimCode)
-- other lines are simply ignored
toDocGroup _ _ = Nothing

trimProperty, trimExample, trimVerbatim :: DocLine -> DocLine
trimProperty = trimPrefix "prop>"
trimExample = trimPrefix ">>>"
trimVerbatim = fromJust . stripPrefix ">" . trimLeft

data LineType
  = Example
  | Property
  | Verbatim
  | Blank
  | Other
  deriving stock (Eq, Show)

pDocLineType :: R.ReadP LineType
pDocLineType
  =   Blank <$ R.eof
  <|> Example <$ R.string ">>>" <* nonGT
  <|> Verbatim <$ R.string ">" <* nonGT
  <|> Property <$ R.string "prop>" <* nonGT
  where nonGT = void (R.satisfy (/= '>')) <|> R.eof

docLineType :: String -> LineType
docLineType s = case R.readP_to_S (R.skipSpaces *> pDocLineType) s of
  []        -> Other
  [(k, _)]  -> k
  _ : _ : _ -> error "impossible: no ambiguity allowed in LineType"

groupByKey :: (a -> k) -> (k -> k -> Bool) -> [a] -> [(k, NonEmpty (k, a))]
groupByKey key cmp
  = map (fst . NonEmpty.head &&& id)
  . NonEmpty.groupBy (cmp `on` fst)
  . map (key &&& id)

assocLineType :: LineType -> LineType -> Bool
-- properties are always one on each line
assocLineType Property _        = False
assocLineType _        Property = False
-- examples are separated by blank lines
-- multiple example program lines can be grouped together
-- in this case the output will be in the same do block
assocLineType Example  Blank    = False
assocLineType Example  Other    = True
assocLineType Example  Example  = True
-- verbatim code is only joined with more verbatim code
assocLineType Verbatim Verbatim = True
assocLineType Verbatim _        = False
-- now the first one is either blank or other, example can be started
assocLineType _        Example  = False
assocLineType _        Verbatim = False
-- and everything else can be grouped and dropped
assocLineType _        _        = True

handleInstruction :: Instruction -> [DocLine] -> Either SetupCode DocTests
handleInstruction instr ls = maybe (Right warning) (handleInstructionNE instr) (nonEmpty ls)
  where warning = Warning noLoc ("instruction " <> show instr <> " with a empty block")

handleInstructionNE :: Instruction -> NonEmpty DocLine -> Either SetupCode DocTests
handleInstructionNE (SetupImport g)   ls = Left mempty{ setupImport = [(g, ls)] }
handleInstructionNE (SetupOther g)    ls = Left mempty{ setupTop = [(g, ls)] }
handleInstructionNE (SetupAuto g)     ls = Left SetupCode{ setupImport = mk imp, setupTop = mk top }
  where (imp, top) = partitionEithers (map isImport (NonEmpty.toList ls))
        isImport l = if "import" `List.isPrefixOf` l.textLine then Left l else Right l
        mk = maybe [] (pure . (g, )) . nonEmpty
handleInstructionNE (UseHook h xs)    ls = Right (TestHook (IOHook h xs ls) [])
handleInstructionNE IsExample         ls = Right (TestMultiline ls)
handleInstructionNE IsProperty        ls = Right (TestProperty ls)
handleInstructionNE (CaptureText x m) ls = Right (Capture (Captured x m (fmap (.textLine) ls)) [])
