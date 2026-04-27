module SongMaker.Render.TeX.LayoutBackend (renderProjectOwnedTeX) where

import Data.Char (isAlphaNum)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import SongMaker.AST.Layout
import SongMaker.AST.Semantic (ChordAnchor (..), ChordSymbol (..), SectionKind (..))

renderProjectOwnedTeX :: SongLayout -> String
renderProjectOwnedTeX sl =
    unlines (songHeader ++ songBody ++ ["\\EndSong"])
  where
    meta = slMeta sl
    songHeader =
        [ "\\BeginSong{" ++ esc (T.unpack (fromMaybe (T.pack "1") (lmSongNumber meta))) ++ "}"
        , "  {" ++ esc (T.unpack (lmTitle meta)) ++ "}"
        , "  {" ++ escT (lmReference meta) ++ "}"
        , "  {" ++ escT (lmAuthorLine meta) ++ "}"
        , "  {" ++ escT (lmCopyrightLine meta) ++ "}"
        , ""
        ]
    songBody = snd (foldl renderBlock (1 :: Int, []) (slBlocks sl))
    escT = maybe "" (esc . T.unpack)

renderBlock :: (Int, [String]) -> LayoutBlock -> (Int, [String])
renderBlock (n, acc) b = case b of
    LBSection sec ->
        let (n', beginLine, endLine) = sectionEnvelope n sec
            body = map (("  " ++) . renderLine) (lsLines sec)
         in (n', acc ++ [beginLine] ++ body ++ [endLine, ""])
    LBNote note -> (n, acc ++ [renderNote note, ""])
    LBSpacer _ -> (n, acc ++ [""])
    LBInterSong t -> (n, acc ++ ["% intersong", T.unpack t, ""])
    LBRawTeX t -> (n, acc ++ [T.unpack t, ""])

sectionEnvelope :: Int -> LayoutSection -> (Int, String, String)
sectionEnvelope n sec
    | lsRole sec == VerseSection =
        let vnum = fromMaybe n (lsOrdinal sec)
         in (vnum + 1, "\\BeginVerse{" ++ show vnum ++ "}", "\\EndVerse")
    | otherwise =
        (n, "\\BeginSection{" ++ esc (T.unpack (fromMaybe (T.pack "") (lsVisibleLabel sec))) ++ "}", "\\EndSection")

renderNote :: LayoutNote -> String
renderNote n = case n of
    LTextNote t -> "\\SongTextNote{" ++ esc (T.unpack t) ++ "}"
    LMusicNote t -> "\\SongMusicNote{" ++ esc (T.unpack t) ++ "}"
    LGenericNote t -> "\\SongTextNote{" ++ esc (T.unpack t) ++ "}"

renderLine :: LayoutLine -> String
renderLine l =
    "\\SongLine{" ++ leftRep ++ embedChords lyric anchors ++ rightRep ++ "}"
  where
    lyric = T.unpack (llLyricText l)
    anchors = sortOn caOffset (llChordAnchors l)
    leftRep = if rmOpen (llRepeatMarkers l) then "\\LRep " else ""
    rightRep = if rmClose (llRepeatMarkers l) then "\\RRep" else ""

embedChords :: String -> [ChordAnchor] -> String
embedChords lyric anchors = go 0 anchors
  where
    n = length lyric
    go i [] = esc (drop i lyric)
    go i (a : as) =
        let off = clamp 0 n (caOffset a)
            prefix = esc (slice i off lyric)
            nextOff = case as of
                [] -> n
                b : _ -> clamp off n (caOffset b)
            (frag, consumed) = takeFragment off nextOff lyric
            chordName = case caChord a of
                ChordSymbol c -> esc (T.unpack c)
         in prefix
                ++ if null frag
                    then "\\ChordOnly{" ++ chordName ++ "}"
                    else "\\Ch{" ++ chordName ++ "}{" ++ esc frag ++ "}"
                ++ go (off + consumed) as

takeFragment :: Int -> Int -> String -> (String, Int)
takeFragment off nextOff lyric
    | off >= length lyric = ("", 0)
    | otherwise =
        let available = max 0 (nextOff - off)
            source = drop off lyric
            maxTake = if available == 0 then length source else available
            raw = take maxTake source
            trimmedLead = dropWhile (== ' ') raw
            leadSpaces = length raw - length trimmedLead
            token = takeWhile isFragChar trimmedLead
            tokLen = length token
            chosenLen =
                if tokLen > 0
                    then leadSpaces + tokLen
                    else
                        if not (null raw)
                            then 1
                            else 0
            frag = take chosenLen raw
         in (frag, chosenLen)
  where
    isFragChar c = isAlphaNum c || c `elem` ("'`-" :: String)

slice :: Int -> Int -> [a] -> [a]
slice a b = take (max 0 (b - a)) . drop a

clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)

esc :: String -> String
esc = concatMap go
  where
    go '\\' = "\\textbackslash{}"
    go '{' = "\\{"
    go '}' = "\\}"
    go '#' = "\\#"
    go '$' = "\\$"
    go '%' = "\\%"
    go '&' = "\\&"
    go '_' = "\\_"
    go '~' = "\\textasciitilde{}"
    go '^' = "\\textasciicircum{}"
    go c = [c]
