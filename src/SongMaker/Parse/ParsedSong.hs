module SongMaker.Parse.ParsedSong (parseSongParsed) where

import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import SongMaker.AST.Parsed
import SongMaker.Common (Stream)
import SongMaker.Read.Chord (isChordsLine)

parseSongParsed :: Stream -> Either String SongParsed
parseSongParsed s = do
    let ls = lines s
        (headerLines, rest) = break isEndMarker ls
        bodyStart = drop 1 rest
        (songBody, trailing) = break isEndMarker bodyStart
        trailingBody = drop 1 trailing
        headerFields = mapMaybe parseHeaderLine headerLines
        title = lookupField "title" headerFields
    songTitle <- maybe (Left "song has no title on songmaker import") Right title
    let meta = mkMeta songTitle headerFields
        sectionBlocks = concatMap parseChunk (splitChunks songBody)
        trailingBlock = mkTrailing trailingBody
    Right $ SongParsed meta (sectionBlocks ++ trailingBlock)
  where
    mkTrailing [] = []
    mkTrailing xs =
        [ BPInterSong
            RawBlock
                { rawBlockText = T.pack (unlines xs)
                , rawBlockOrigin = InterSongOrigin
                }
        ]

isEndMarker :: String -> Bool
isEndMarker l = "***" `isPrefixOf` dropWhile isSpace l

parseHeaderLine :: String -> Maybe (Text, Text)
parseHeaderLine l = case break (== ':') l of
    (k, ':' : v) ->
        let key = T.strip (T.pack k)
            val = T.strip (T.pack v)
         in if T.null key || T.null val then Nothing else Just (key, val)
    _ -> Nothing

lookupField :: Text -> [(Text, Text)] -> Maybe Text
lookupField key = fmap snd . safeHead . filter ((== key) . fst)

lookupMany :: Text -> [(Text, Text)] -> [Text]
lookupMany key = map snd . filter ((== key) . fst)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

mkMeta :: Text -> [(Text, Text)] -> SongMetaParsed
mkMeta title fields =
    SongMetaParsed
        { smTitle = title
        , smOriginal = lookupField "original" fields
        , smAuthor = lookupField "author" fields
        , smLyricsBy = lookupField "lyricsBy" fields
        , smMusicBy = lookupField "musicBy" fields
        , smTranslationBy = lookupField "translationBy" fields
        , smKey = lookupField "key" fields
        , smCopyright = lookupField "copyright" fields
        , smReference = lookupField "reference" fields
        , smLicense = lookupField "license" fields
        , smNumbering = numbering
        , smExtraIndex = lookupMany "extra-index" fields
        , smExtraTitleIndex = lookupMany "extra-title-index" fields
        , smUnknownFields = filter (not . (`elem` knownHeaders) . fst) fields
        }
  where
    numbering = case fmap T.toLower (lookupField "numbering" fields) of
        Just "numbered" -> Just NumberedParsed
        Just "chorus" -> Just ChorusOnlyParsed
        Just "unnumbered" -> Just UnnumberedParsed
        _ -> Nothing
    knownHeaders =
        [ "title"
        , "original"
        , "author"
        , "lyricsBy"
        , "musicBy"
        , "translationBy"
        , "key"
        , "copyright"
        , "reference"
        , "license"
        , "numbering"
        , "extra-index"
        , "extra-title-index"
        ]

splitChunks :: [String] -> [[String]]
splitChunks = go [] []
  where
    go acc cur [] = reverse (flush acc cur)
    go acc cur (l : ls)
        | all isSpace l = go (flush acc cur) [] ls
        | otherwise = go acc (cur ++ [l]) ls
    flush acc [] = acc
    flush acc cur = cur : acc

parseChunk :: [String] -> [BlockParsed]
parseChunk [] = []
parseChunk ls
    | isInterSongBlock ls = [BPInterSong (RawBlock (T.pack (unlines ls)) InterSongOrigin)]
    | all isNoteLine ls = map (BPNote . parseNoteLine . T.pack) ls
    | all isRawTeXLine ls = [BPRawTeX (RawBlock (T.pack (unlines ls)) RawTeXOrigin)]
    | otherwise = [BPSection (parseSection ls)]

isInterSongBlock :: [String] -> Bool
isInterSongBlock ls = case ls of
    [] -> False
    _ ->
        let first = dropWhile isSpace (head ls)
            lastL = dropWhile isSpace (last ls)
         in "\\begin{intersong}" `isPrefixOf` first && "\\end{intersong}" `isPrefixOf` lastL

isRawTeXLine :: String -> Bool
isRawTeXLine l = "\\" `isPrefixOf` dropWhile isSpace l

isNoteLine :: String -> Bool
isNoteLine = ("!" `isPrefixOf`) . dropWhile isSpace

parseNoteLine :: Text -> NoteParsed
parseNoteLine t =
    let body = T.strip . T.dropWhile (== '!') . T.dropWhile isSpaceText $ t
        low = T.toLower body
     in if "text:" `T.isPrefixOf` low
            then TextNote (T.strip (T.drop 5 body))
            else
                if "music:" `T.isPrefixOf` low
                    then MusicNote (T.strip (T.drop 6 body))
                    else GenericNote body
  where
    isSpaceText = (== ' ')

parseSection :: [String] -> SectionParsed
parseSection ls =
    let lineTexts = map T.pack ls
        (lbl, src, lineTexts') = parseLabel lineTexts
     in SectionParsed
            { secParsedLabel = lbl
            , secParsedLines = parseLines lineTexts'
            , secParsedSource = src
            }

parseLabel :: [Text] -> (Maybe Text, SectionSourceKind, [Text])
parseLabel [] = (Nothing, SourceUnknown, [])
parseLabel (l : ls) =
    let t = T.stripStart l
        pref = labelPrefixes
        match = safeHead [x | x <- pref, fst x `T.isPrefixOf` T.toLower t]
     in case match of
            Nothing -> (Nothing, SourceVerse, l : ls)
            Just (p, k) ->
                let rest = T.stripStart (T.drop (T.length p) t)
                    line' = if T.null rest then ls else rest : ls
                 in (Just (T.take (T.length p) t), k, line')

labelPrefixes :: [(Text, SectionSourceKind)]
labelPrefixes =
    [ ("refrain:", SourceChorus)
    , ("refrain", SourceChorus)
    , ("refr.:", SourceChorus)
    , ("refr.", SourceChorus)
    , ("refr", SourceChorus)
    , ("ref.:", SourceChorus)
    , ("ref.", SourceChorus)
    , ("ref ", SourceChorus)
    , ("r.:", SourceChorus)
    , ("chorus:", SourceChorus)
    , ("chorus", SourceChorus)
    , ("chor.:", SourceChorus)
    , ("chor.", SourceChorus)
    , ("chor ", SourceChorus)
    , ("bridge:", SourceBridge)
    , ("bridge", SourceBridge)
    , ("br.:", SourceBridge)
    , ("br.", SourceBridge)
    , ("br ", SourceBridge)
    ]

parseLines :: [Text] -> [LineParsed]
parseLines [] = []
parseLines [x]
    | hasCaret x = [LPCaretLyric x]
    | hasInlineChord x = [LPInlineChordLyric x]
    | otherwise = [LPInlineChordLyric x]
parseLines (x : y : rest)
    | isChordsLine (T.unpack x) =
        LPChordLyric x y : parseLines rest
    | hasCaret x = LPCaretLyric x : parseLines (y : rest)
    | hasInlineChord x = LPInlineChordLyric x : parseLines (y : rest)
    | otherwise = LPInlineChordLyric x : parseLines (y : rest)

hasCaret :: Text -> Bool
hasCaret = T.any (== '^')

hasInlineChord :: Text -> Bool
hasInlineChord t = "\\[" `T.isInfixOf` t && "]" `T.isInfixOf` t

parseOrdinal :: Text -> Maybe Int
parseOrdinal t = case TR.decimal (T.takeWhile (\c -> c >= '0' && c <= '9') (T.strip t)) of
    Right (n, _) -> Just n
    Left _ -> Nothing

