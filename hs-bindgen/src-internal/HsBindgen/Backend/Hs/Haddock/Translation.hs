{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Backend.Hs.Haddock.Translation (
    mkHaddocks
  , mkHaddocksFieldInfo
  , mkHaddocksDecorateParams
  , peelCategoryComment
  ) where

import Data.Text qualified as Text
import System.FilePath (takeFileName)

import Clang.HighLevel.Types qualified as C
import Clang.Paths qualified as C

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Haddock.Config (HaddockConfig (..), PathStyle (..))
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Translation
import HsBindgen.Language.Haskell qualified as Hs

import Doxygen.Parser.Types (ParamDirection (..), ParamListKind (..),
                             SimpleSectKind (..))
import Doxygen.Parser.Types qualified as Doxy

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

-- | Convert a Doxygen comment to a Haddock comment
--
mkHaddocks ::
     HaddockConfig
  -> C.DeclInfo Final
  -> Maybe HsDoc.Comment
mkHaddocks config info =
    fst $ mkHaddocksWithArgs config info Args{
        isField = False
      , loc     = info.loc
      , cName   = C.renderDeclId info.id.cName
      , hsName  = info.id.hsName
      , comment = info.comment
      , params  = []
      }

mkHaddocksFieldInfo ::
     HaddockConfig
  -> C.DeclInfo Final
  -> C.FieldInfo Final
  -> Maybe HsDoc.Comment
mkHaddocksFieldInfo config declInfo fieldInfo =
    fst $ mkHaddocksWithArgs config declInfo Args{
        isField = True
      , loc     = fieldInfo.loc
      , cName   = fieldInfo.name.cName.text
      , hsName  = fieldInfo.name.hsName
      , comment = fieldInfo.comment
      , params  = []
      }

-- | Extract Haddock documentation for a function; enrich function parameters
--   with parameter-specific documentation
mkHaddocksDecorateParams ::
     HaddockConfig
  -> C.DeclInfo Final
  -> [(Maybe Text, Hs.FunctionParameter)]
  -> (Maybe HsDoc.Comment, [Hs.FunctionParameter])
mkHaddocksDecorateParams config info params =
    let (mbc, xs) = mkHaddocksWithArgs config info Args{
        isField = False
      , loc     = info.loc
      , cName   = C.renderDeclId info.id.cName
      , hsName  = info.id.hsName
      , comment = info.comment
      , params  = params
      }
    in  (mbc, xs)

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

data Args = Args{
      isField :: Bool
    , loc     :: C.SingleLoc
    , cName   :: Text
    , hsName  :: Hs.SomeName
    , comment :: Maybe (C.Comment Final)
    , params  :: [(Maybe Text, Hs.FunctionParameter)]
    }

-- | Convert a Doxygen comment to a Haddock comment, updating function
-- parameters with their documentation.
--
mkHaddocksWithArgs :: HaddockConfig -> C.DeclInfo Final -> Args -> (Maybe HsDoc.Comment, [Hs.FunctionParameter])
mkHaddocksWithArgs HaddockConfig{..} info Args{comment = Nothing, ..} =
      ( Just $
          mempty
            & #origin     .~ Just cName
            & #location   .~ Just (updateSingleLoc pathStyle loc)
            & #headerInfo .~ Just info.headerInfo
      , map (uncurry addFunctionParameterComment) params
      )
mkHaddocksWithArgs HaddockConfig{..} info Args{comment = Just (C.Comment Doxy.Comment{..}), ..} =
  let commentCName    = cName
      commentLocation = updateSingleLoc pathStyle loc

      -- The brief description becomes the Haddock title
      commentTitle = case brief of
        []      -> Nothing
        inlines -> Just $ concatMap convertInline inlines

      -- Extract param docs from detailed blocks for attaching to
      -- function parameters
      paramDocs = extractMatchedParams detailed

      -- Match param docs to function parameters
      updatedParams = map (uncurry addFunctionParameterComment)
                    . processParamDocs paramDocs
                    $ params

      -- Convert detailed blocks to Haddock content.
      -- Matched params render on the arguments themselves; keeping a body
      -- copy too would duplicate every entry, so the body keeps only the
      -- UNMATCHED ones (typedef'd callback params, parse strays), which
      -- have nowhere else to appear. Adjacent \sa sections coalesce into
      -- one "See also" item first.
      commentChildren = concatMap convertBlock (coalesceSees (mapMaybe dropMatchedParams detailed))

      -- When doxygen puts all text in detailed (e.g. inline enum comments
      -- like /**< Red color */), promote the first simple paragraph to title.
      (finalTitle, finalChildren) = case (commentTitle, commentChildren) of
        (Nothing, HsDoc.Paragraph inlines : rest)
          | all isSimpleInline inlines -> (Just inlines, rest)
        _ -> (commentTitle, commentChildren)

  in  ( Just $
          mempty
            & #title      .~ finalTitle
            & #origin     .~ Just commentCName
            & #location   .~ Just commentLocation
            & #headerInfo .~ Just info.headerInfo
            & #children   .~ finalChildren
      , updatedParams
      )
  where
    -- One predicate for both the extraction into per-argument comments and
    -- the body-side drop: whatever is extracted is exactly what the body
    -- omits, so the two can never disagree about where a param doc lives.
    isMatchedParam :: Doxy.Param (C.CommentRef Final) -> Bool
    isMatchedParam p = any ((== Just p.paramName) . fst) params

    -- Extract @param entries from detailed blocks that match provided parameters
    extractMatchedParams :: [Doxy.Block (C.CommentRef Final)]
                         -> [Doxy.Param (C.CommentRef Final)]
    extractMatchedParams [] = []
    extractMatchedParams (Doxy.ParamList ParamListParam ps : rest) =
        filter isMatchedParam ps ++ extractMatchedParams rest
    extractMatchedParams (_ : rest) = extractMatchedParams rest

    -- Drop matched params from a body block; a param list whose every
    -- entry moved onto an argument disappears entirely.
    dropMatchedParams :: Doxy.Block (C.CommentRef Final)
                      -> Maybe (Doxy.Block (C.CommentRef Final))
    dropMatchedParams = \case
      Doxy.ParamList ParamListParam ps ->
        case filter (not . isMatchedParam) ps of
          []  -> Nothing
          ps' -> Just (Doxy.ParamList ParamListParam ps')
      block -> Just block

    processParamDocs :: [Doxy.Param (C.CommentRef Final)]
                     -> [(Maybe Text, Hs.FunctionParameter)]
                     -> [(Maybe Text, Hs.FunctionParameter)]
    processParamDocs [] currentParams = currentParams
    processParamDocs (dp : rest) currentParams =
        processParamDocs rest $ map (updateParam dp) currentParams
      where
        updateParam dp' (mbName, fp)
          | mbName == Just (Text.strip dp'.paramName)
          = let paramComment :: HsDoc.Comment
                paramComment = mempty
                  & #origin   .~ mbName
                  -- Position 1 is never consulted: a matched name is
                  -- necessarily non-empty.
                  & #children .~ [convertParam ParamListParam 1 dp']
            in  (mbName, fp & #comment .~ Just paramComment)
          | otherwise = (mbName, fp)

addFunctionParameterComment :: Maybe Text -> Hs.FunctionParameter -> Hs.FunctionParameter
addFunctionParameterComment mbName fp =
  case mbName of
    Nothing -> fp
    Just name
      | Text.null name -> panicPure "Function parameter name is null"
      | otherwise ->
        case fp.comment of
          Just{}  -> fp
          Nothing -> fp & #comment .~ Just (
              mempty & #origin .~ mbName
            )

{-------------------------------------------------------------------------------
  Category overview peeling
-------------------------------------------------------------------------------}

-- | Split an SDL-style category overview off the first declaration.
--
-- SDL headers open with a file-level comment (@\/** # CategoryInit ... *\/@)
-- that doxygen cannot attach to the file: it fuses the whole block into the
-- first declaration's detailed description, where the markdown heading
-- surfaces as a @\<sect1\>@ whose title is the @CategoryX@ marker. Detection
-- is purely structural: the first detailed block of the first declaration's
-- comment must be a @sect1@ tag whose title text matches
-- @^Category[A-Za-z0-9_]+$@ — no prose heuristics.
--
-- The peeled overview (minus the @CategoryX@ title, with the first simple
-- paragraph promoted to the comment title so it renders as the Haddock
-- module description) is returned alongside the declarations with that
-- block removed. Note that doxygen may have fused the tail of the first
-- declaration's own documentation into the section; the fusion is not
-- recoverable, so such text moves to the module comment with the overview.
--
-- Callers must apply both halves consistently: the backend translates the
-- peeled declarations, and module translation receives the module comment.
peelCategoryComment ::
     [C.Decl l Final]
  -> (Maybe HsDoc.Comment, [C.Decl l Final])
peelCategoryComment = \case
    decl : decls
      | Just (C.Comment doxy) <- decl.info.comment
      , Doxy.Tag "sect1" (titleBlock : overview) : rest <- doxy.detailed
      , isCategoryTitle titleBlock ->
          let comment' = case (doxy.brief, rest) of
                ([], []) -> Nothing
                _        -> Just (C.Comment doxy{Doxy.detailed = rest})
          in  ( Just $ overviewComment overview
              , (decl & #info % #comment .~ comment') : decls
              )
    decls -> (Nothing, decls)
  where
    -- The @<sect1>@ title as parsed by doxygen-parser: a @title@ tag holding
    -- one paragraph of plain text, the category marker.
    isCategoryTitle :: Doxy.Block (C.CommentRef Final) -> Bool
    isCategoryTitle = \case
      Doxy.Tag "title" [Doxy.Paragraph [Doxy.Text t]] -> isCategoryMarker (Text.strip t)
      _                                               -> False

    isCategoryMarker :: Text -> Bool
    isCategoryMarker t = case Text.stripPrefix "Category" t of
      Just rest -> not (Text.null rest) && Text.all isMarkerChar rest
      Nothing   -> False

    isMarkerChar :: Char -> Bool
    isMarkerChar c =
         ('A' <= c && c <= 'Z')
      || ('a' <= c && c <= 'z')
      || ('0' <= c && c <= '9')
      || c == '_'

    -- Same promotion as 'mkHaddocksWithArgs': the first simple paragraph
    -- becomes the title, which the pretty-printer renders on the @{-|@ line
    -- (Haddock's module description).
    overviewComment :: [Doxy.Block (C.CommentRef Final)] -> HsDoc.Comment
    overviewComment blocks = case concatMap convertBlock (coalesceSees blocks) of
      HsDoc.Paragraph inlines : rest
        | all isSimpleInline inlines ->
            mempty
              & #title    .~ Just inlines
              & #children .~ rest
      children ->
            mempty
              & #children .~ children

{-------------------------------------------------------------------------------
  Block content conversion

  Doxy.Block maps directly to HsDoc.CommentBlockContent.  No string matching
  on command names — the Doxygen XML has already classified everything.
-------------------------------------------------------------------------------}

-- | Convert a 'Doxy.Block' to Haddock block content
convertBlock :: Doxy.Block (C.CommentRef Final) -> [HsDoc.CommentBlockContent]
convertBlock = \case
  Doxy.Paragraph inlines ->
    [HsDoc.Paragraph $ concatMap convertInline inlines]

  Doxy.ParamList kind ps ->
    case kind of
      ParamListParam  -> zipWith (convertParam kind) [1 ..] ps
      ParamListRetVal -> concatMap convertRetval ps

  Doxy.SimpleSect kind blocks ->
    convertSimpleSect kind blocks

  Doxy.CodeBlock codeLines ->
    [HsDoc.CodeBlock codeLines]

  Doxy.ItemizedList items ->
    map (\item -> HsDoc.ListItem HsDoc.BulletList (concatMap convertBlock item)) items

  Doxy.OrderedList items ->
    zipWith (\i item -> HsDoc.ListItem (HsDoc.NumberedList i)
                                       (concatMap convertBlock item))
            [1..] items

  Doxy.XRefSect title blocks ->
    defItem (Text.dropWhileEnd (== ':') (Text.strip title)) (concatMap convertBlock blocks)

  Doxy.Tag _tag children -> concatMap convertBlock children

-- | Convert a @\<simplesect\>@ to Haddock content
convertSimpleSect :: SimpleSectKind -> [Doxy.Block (C.CommentRef Final)] -> [HsDoc.CommentBlockContent]
convertSimpleSect kind blocks =
    let content = concatMap convertBlock blocks
    in  case kind of
          SSReturn     -> defItem "Returns" content
          SSWarning    -> defItem "Warning" content
          SSNote       -> defItem "Note" content
          SSSee        -> defItem "See also" content
          SSSince      ->
            -- Haddock's @since expects a bare version token; prose like
            -- "This function is available since SDL 3.2.0." renders as
            -- garbage there. Extract the version when present, otherwise
            -- fall back to an ordinary "Since" item.
            case findVersionToken (extractText blocks) of
              Just ver -> [HsDoc.Paragraph [HsDoc.Metadata (HsDoc.Since ver)]]
              Nothing  -> defItem "Since" content
          SSVersion    -> defItem "Version" content
          SSPre        -> defItem "Precondition" content
          SSPost       -> defItem "Postcondition" content
          -- The \par title carries its own label ("Thread safety:"); as a
          -- definition item the label and its content stay in ONE block
          -- instead of a bold label paragraph orphaned from its text.
          SSPar title  -> defItem (Text.dropWhileEnd (== ':') (Text.strip title)) content
          SSDeprecated -> defItem "Deprecated" content
          SSRemark     -> defItem "Remark" content
          SSAttention  -> defItem "Attention" content
          SSTodo       -> defItem "TODO" content
          SSInvariant  -> defItem "Invariant" content
          SSAuthor     -> defItem "Author" content
          SSDate       -> defItem "Date" content
  where
    extractText :: [Doxy.Block (C.CommentRef Final)] -> Text
    extractText = Text.strip . Text.unwords . concatMap go
      where
        go (Doxy.Paragraph inlines) = map inlineText inlines
        go _                       = []

        inlineText (Doxy.Text t) = t
        inlineText _            = ""

    -- First whitespace-separated token shaped like a version: digit
    -- groups joined by dots (at least one dot), trailing sentence
    -- punctuation dropped ("3.2.0." -> "3.2.0").
    findVersionToken :: Text -> Maybe Text
    findVersionToken t =
        case filter isVersion (map trim (Text.words t)) of
          (v : _) -> Just v
          []      -> Nothing
      where
        trim = Text.dropWhileEnd (== '.')
        digit c = c >= '0' && c <= '9'
        isVersion w =
             Text.elem '.' w
          && Text.all (\c -> digit c || c == '.') w
          && all (\g -> not (Text.null g) && Text.all digit g)
                 (Text.splitOn "." w)

-- | Check if an inline content element is simple enough to promote to a title
isSimpleInline :: HsDoc.CommentInlineContent -> Bool
isSimpleInline = \case
  HsDoc.TextContent{} -> True
  HsDoc.Monospace{}   -> True
  HsDoc.Emph{}        -> True
  HsDoc.Bold{}        -> True
  HsDoc.Identifier{}  -> True
  HsDoc.Module{}      -> True
  HsDoc.Link{}        -> True
  _                   -> False

-- | Render a labelled section as one definition-list item: a quiet label
-- column instead of a bold run per section, and the label can never be
-- orphaned from its content.
defItem :: Text -> [HsDoc.CommentBlockContent] -> [HsDoc.CommentBlockContent]
defItem label cs = [HsDoc.DefinitionList (HsDoc.TextContent label) cs]

-- | Merge each run of adjacent @\\sa@ sections into a single section whose
-- one paragraph comma-joins the references, so a function's see-also block
-- renders as one definition item instead of a labelled paragraph per
-- reference. A run member that is not exactly one paragraph disables
-- coalescing for its run (never observed; \\sa carries a reference list).
coalesceSees :: [Doxy.Block r] -> [Doxy.Block r]
coalesceSees = \case
    [] -> []
    b : rest
      | Just first <- seeParagraph b
      , (mores, rest') <- spanJust seeParagraph rest
      , not (null mores) ->
          Doxy.SimpleSect SSSee
            [Doxy.Paragraph (foldl' joinWithComma first mores)]
            : coalesceSees rest'
    b : rest -> b : coalesceSees rest
  where
    seeParagraph = \case
      Doxy.SimpleSect SSSee [Doxy.Paragraph inlines] -> Just inlines
      _ -> Nothing

    -- The renderer's punctuation rule attaches a leading-punctuation text
    -- node directly to the preceding element, so the comma lands snug.
    joinWithComma acc inlines = acc <> (Doxy.Text "," : inlines)

    spanJust :: (a -> Maybe b) -> [a] -> ([b], [a])
    spanJust f = go
      where
        go [] = ([], [])
        go (x : xs) = case f x of
          Just y  -> let (ys, zs) = go xs in (y : ys, zs)
          Nothing -> ([], x : xs)

-- | Convert a documented parameter to a Haddock definition list entry;
-- the positional index is the term of last resort when the parameter
-- name is missing (e.g. an upstream doxygen XML quirk).
convertParam :: ParamListKind -> Int -> Doxy.Param (C.CommentRef Final) -> HsDoc.CommentBlockContent
convertParam _kind position p =
    let name = Text.strip p.paramName
        paramNameContent
          | Text.null name = HsDoc.TextContent ("arg " <> Text.pack (show position))
          | otherwise      = HsDoc.Monospace [HsDoc.TextContent name]
        desc = concatMap convertBlock p.paramDesc
        -- Direction annotations, when present, lead the description; the
        -- term itself stays a bare code span.
        withDir = case dirInline p.paramDirection of
          []   -> desc
          dirs -> HsDoc.Paragraph dirs : desc
    in  HsDoc.DefinitionList paramNameContent withDir

-- | Convert a @\@retval@ entry to a definition list item:
--   @[@value@]: description@
convertRetval :: Doxy.Param (C.CommentRef Final) -> [HsDoc.CommentBlockContent]
convertRetval p =
    let code = HsDoc.Monospace [HsDoc.TextContent (Text.strip p.paramName)]
        desc = concatMap convertBlock p.paramDesc
    in  [HsDoc.DefinitionList code desc]

-- | Direction annotation for a parameter (empty list when unspecified)
dirInline :: Maybe ParamDirection -> [HsDoc.CommentInlineContent]
dirInline (Just DirIn)    = [HsDoc.Emph [HsDoc.TextContent "(input)"]]
dirInline (Just DirOut)   = [HsDoc.Emph [HsDoc.TextContent "(output)"]]
dirInline (Just DirInOut) = [HsDoc.Emph [HsDoc.TextContent "(input,output)"]]
dirInline Nothing         = []

{-------------------------------------------------------------------------------
  Inline content conversion

  Doxy.Inline maps directly to HsDoc.CommentInlineContent.
  Cross-references (Doxy.Ref) are resolved via the CommentRef that was
  threaded through the pipeline and resolved by MangleNames.
-------------------------------------------------------------------------------}

-- | Convert a 'Doxy.Inline' to Haddock inline content
--
-- Text content is stripped because the Haddock pretty-printer uses 'hsep'
-- to join inline elements (adding its own inter-element spacing).
-- The Doxygen parser preserves XML whitespace in text nodes, so we strip
-- it here at the translation boundary.
--
convertInline :: Doxy.Inline (C.CommentRef Final) -> [HsDoc.CommentInlineContent]
convertInline = \case
  Doxy.Text t
    | Text.null stripped -> []
    | otherwise          -> [HsDoc.TextContent stripped]
    where stripped = Text.strip t

  Doxy.Bold inlines  -> [HsDoc.Bold $ concatMap convertInline inlines]
  Doxy.Emph inlines  -> [HsDoc.Emph $ concatMap convertInline inlines]
  -- Un-nest monospace inside monospace (a code span holding an unresolved
  -- cross-reference): nested Monospace prints as doubled @ delimiters,
  -- which Haddock mis-parses into literal @ characters around the span.
  Doxy.Mono inlines  -> [HsDoc.Monospace $ flattenMono $ concatMap convertInline inlines]

  Doxy.Ref (C.CommentRef c mHsIdent _mKind) _displayText ->
    case mHsIdent of
      Just namePair -> [HsDoc.Identifier namePair.hsName.text]
      Nothing       -> [HsDoc.Monospace [HsDoc.TextContent c]]

  Doxy.Anchor idText -> [HsDoc.Anchor idText]

  Doxy.Link label url -> [HsDoc.Link (concatMap convertInline label) url]

-- | Splice nested monospace content into its parent code span.
flattenMono :: [HsDoc.CommentInlineContent] -> [HsDoc.CommentInlineContent]
flattenMono = concatMap $ \case
  HsDoc.Monospace inner -> flattenMono inner
  other                 -> [other]

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Depending on the configured 'PathStyle', update 'HsBindgen.Clang.HighLevel.Types.SingleLoc'
-- to either have a short or full path name.
--
-- See #966.
updateSingleLoc :: PathStyle -> C.SingleLoc -> C.SingleLoc
updateSingleLoc Short C.SingleLoc{..} =
  C.SingleLoc {
    singleLocPath = C.SourcePath
                  . Text.pack
                  . takeFileName
                  . C.getSourcePath
                  $ singleLocPath
  , ..
  }
updateSingleLoc _     sloc = sloc
