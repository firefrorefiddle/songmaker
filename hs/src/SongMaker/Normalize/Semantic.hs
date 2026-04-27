module SongMaker.Normalize.Semantic (normalizeSong) where

import Data.Char (isDigit, isSpace, toLower)
import Data.List (find, isPrefixOf)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import SongMaker.AST.Parsed
import SongMaker.AST.Semantic
import SongMaker.Read.Chord (chordsFromLine)

normalizeSong :: SongParsed -> SongSemantic
normalizeSong sp =
    let (elements, warnings, _) = foldl normalizeBlock ([], [], []) (spBlocks sp)
     in SongSemantic
            { ssMeta = normalizeMeta (spMeta sp)
            , ssElements = reverse elements
            , ssWarnings = reverse warnings
            }

normalizeMeta :: SongMetaParsed -> SongMetaSemantic
normalizeMeta m =
    SongMetaSemantic
        { sTitle = smTitle m
        , sSubtitle = smOriginal m
        , sAuthors =
            AuthorInfo
                { saAuthor = smAuthor m
                , saLyricsBy = smLyricsBy m
                , saMusicBy = smMusicBy m
                , saTranslationBy = smTranslationBy m
                , saOriginalTitle = smOriginal m
                }
        , sCopyright = smCopyright m
        , sReference = smReference m
        , sLicense = smLicense m
        , sIndexes = smExtraIndex m ++ smExtraTitleIndex m
        , sNumbering = case smNumbering m of
            Just NumberedParsed -> NumberedSemantic
            Just ChorusOnlyParsed -> ChorusOnlySemantic
            Just UnnumberedParsed -> UnnumberedSemantic
            Nothing -> NumberedSemantic
        }

type Acc = ([SongElement], [SongWarning], [SectionSemantic])

normalizeBlock :: Acc -> BlockParsed -> Acc
normalizeBlock (els, ws, prevSecs) blk = case blk of
    BPSection sec ->
        let (secSem, ws') = normalizeSection prevSecs sec
         in (ESection secSem : els, ws' ++ ws, secSem : prevSecs)
    BPNote n ->
        let n' = case n of
                TextNote t -> STextNote t
                MusicNote t -> SMusicNote t
                GenericNote t -> SGenericNote t
         in (ENote n' : els, ws, prevSecs)
    BPInterSong rb -> (EInterSong (rawBlockText rb) : els, ws, prevSecs)
    BPRawTeX rb -> (ERawTeX (rawBlockText rb) : els, ws, prevSecs)

normalizeSection :: [SectionSemantic] -> SectionParsed -> (SectionSemantic, [SongWarning])
normalizeSection prev sec =
    let (lines', ws) = normalizeLines prev (secParsedLines sec)
        kind = classifySection (secParsedSource sec) (secParsedLabel sec)
        ordVal = secParsedLabel sec >>= parseOrdinal
        hasRepeat = any (\l -> mlRepeatOpen l || mlRepeatClose l) lines'
        rep = if hasRepeat then Just RepeatBar else Nothing
     in ( SectionSemantic kind (secParsedLabel sec) ordVal lines' rep
        , ws
        )

normalizeLines :: [SectionSemantic] -> [LineParsed] -> ([MusicalLine], [SongWarning])
normalizeLines prevSecs ls =
    let fallback = find (\s -> length (sectionLines s) == length ls) prevSecs
     in go 0 fallback ls
  where
    go _ _ [] = ([], [])
    go idx fallback (x : xs) =
        let (line', ws1) = normalizeLine idx fallback x
            (rest, ws2) = go (idx + 1) fallback xs
         in (line' : rest, ws1 ++ ws2)

normalizeLine :: Int -> Maybe SectionSemantic -> LineParsed -> (MusicalLine, [SongWarning])
normalizeLine _ _ (LPChordLyric chord lyric) =
    let anchors = mapMaybe mkAnchor (chordsFromLine (T.unpack chord))
        (lyric', ro, rc) = extractRepeatMarkers lyric
     in ( MusicalLine lyric' anchors ro rc []
        , []
        )
  where
    mkAnchor (i, c)
        | null c = Nothing
        | otherwise =
            Just $
                ChordAnchor
                    { caOffset = max 0 i
                    , caChord = ChordSymbol (T.pack c)
                    }
normalizeLine idx fallback (LPCaretLyric t) =
    let (lyric', positions) = removeCarets t
        (lyric'', ro, rc) = extractRepeatMarkers lyric'
     in case fallback >>= lineAt idx of
            Nothing ->
                ( MusicalLine lyric'' [] ro rc [t]
                , [UnresolvedCaretPattern t]
                )
            Just src ->
                let srcAnchors = mlAnchors src
                 in if length positions == length srcAnchors
                        then
                            ( MusicalLine lyric'' (zipWith inherit positions srcAnchors) ro rc []
                            , []
                            )
                        else
                            ( MusicalLine lyric'' [] ro rc [t]
                            , [UnresolvedCaretPattern t]
                            )
  where
    inherit pos a = ChordAnchor pos (caChord a)
normalizeLine _ _ (LPInlineChordLyric t) =
    let (lyricRaw, anchors) = extractInline t
        (lyric', ro, rc) = extractRepeatMarkers lyricRaw
     in (MusicalLine lyric' anchors ro rc [], [])
normalizeLine _ _ (LPRawLine t) =
    let (lyric', ro, rc) = extractRepeatMarkers t
     in (MusicalLine lyric' [] ro rc [t], [UnsupportedInlineTeX t])

lineAt :: Int -> SectionSemantic -> Maybe MusicalLine
lineAt idx sec
    | idx < 0 || idx >= length (sectionLines sec) = Nothing
    | otherwise = Just (sectionLines sec !! idx)

extractRepeatMarkers :: Text -> (Text, Bool, Bool)
extractRepeatMarkers t =
    let trimmed = T.strip t
        (open, t1) =
            if "|:" `T.isPrefixOf` trimmed
                then (True, T.stripStart (T.drop 2 trimmed))
                else (False, trimmed)
        (close, t2) =
            if ":|" `T.isSuffixOf` t1
                then (True, T.stripEnd (T.dropEnd 2 t1))
                else (False, t1)
     in (t2, open, close)

removeCarets :: Text -> (Text, [Int])
removeCarets = go 0 [] []
  where
    go _ accPos accChars t
        | T.null t = (T.pack (reverse accChars), reverse accPos)
    go offset accPos accChars t =
        let c = T.head t
            rest = T.tail t
         in if c == '^'
                then go offset (offset : accPos) accChars rest
                else go (offset + 1) accPos (c : accChars) rest

extractInline :: Text -> (Text, [ChordAnchor])
extractInline = go 0 [] []
  where
    go _ chars anchors txt
        | T.null txt = (T.pack (reverse chars), reverse anchors)
    go offset chars anchors txt
        | "\\[" `T.isPrefixOf` txt =
            let rest = T.drop 2 txt
                (ch, rem') = T.breakOn "]" rest
             in if T.null rem'
                    then go (offset + 2) ('[' : '\\' : chars) anchors rest
                    else
                        let after = T.drop 1 rem'
                            anchor = ChordAnchor offset (ChordSymbol ch)
                         in go offset chars (anchor : anchors) after
        | otherwise =
            let c = T.head txt
             in go (offset + 1) (c : chars) anchors (T.tail txt)

classifySection :: SectionSourceKind -> Maybe Text -> SectionKind
classifySection src mLabel = case src of
    SourceChorus -> ChorusSection
    SourceBridge -> BridgeSection
    SourceVerse -> fromLabel
    SourceUnknown -> fromLabel
  where
    fromLabel = case fmap (map toLower . T.unpack) mLabel of
        Just l
            | any (`isPrefixOf` l) ["ref", "refr", "chor"] -> ChorusSection
            | any (`isPrefixOf` l) ["bridge", "br"] -> BridgeSection
            | "intro" `isPrefixOf` l -> IntroSection
            | "outro" `isPrefixOf` l -> OutroSection
            | "pre" `isPrefixOf` l -> PreChorusSection
            | "tag" `isPrefixOf` l -> TagSection
            | otherwise -> VerseSection
        Nothing -> VerseSection

parseOrdinal :: Text -> Maybe Int
parseOrdinal t =
    let digits = T.takeWhile isDigit (T.dropWhile (not . isDigit) t)
     in if T.null digits then Nothing else Just (read (T.unpack digits))
