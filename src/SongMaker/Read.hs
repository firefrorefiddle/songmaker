module SongMaker.Read (readStream, module X) where

import SongMaker.Read.Chord  as X
import SongMaker.Read.Notes  as X
import SongMaker.Read.Song   as X
import SongMaker.Read.Header as X
import SongMaker.Common

import Control.Applicative
import Data.List
import Data.Char
import Data.Maybe

readStream :: Stream -> Either String Song
readStream s =
  let header = readHeader s
      (verseLines, afterSong) = break isEndLine . skipHeader $ s
      rawVerses = filter (nall isSpace . concat) .
                  groupBy (\x y -> all  isSpace x && all  isSpace y ||
                                   nall isSpace x && nall isSpace y) $ verseLines
  in do
    title <- case lookup "title" header of
                 Just title -> return title
                 Nothing    -> Left "song has no title on songmaker import"
    let ret = Song
           { songTitle             = title
           , songOrigTitle         = lookup "original" header
           , songAuthorLyrics      = case lookup "author" header of
                                      Nothing -> lookup "lyricsBy" header
                                      Just author -> Just author
           , songAuthorMusic       = case lookup "author" header of
                                      Nothing -> lookup "musicBy" header
                                      Just author -> Just author
           , songAuthorTranslation = lookup "translationBy" header
           , songCopyright         = lookup "copyright" header
           , songLicense           = lookup "license" header
           , songScriptureRef      = lookup "reference" header
           , songKey               = lookup "key" header
           , songNumbering = case lookup "numbering" header of
                              Nothing -> Numbered
                              Just "numbered" -> Numbered
                              Just "chorus" -> ChorusOnly
                              Just "unnumbered" -> Unnumbered
                              Just other -> warn ("ignoring unknown numbering " ++ other) Numbered
           , songVerses = map (mkVerse ret) rawVerses
           , songAfter = if null afterSong
                         then ""
                         else unlines (tail afterSong)
           , songAltTitles = map snd . filter ((== "extra-title-index").fst) $ header
           , songLineIndexes = map snd . filter ((== "extra-index").fst) $ header
           }
    return ret
  where nall p = not . all p
        mkVerse s v = let firstLy = firstLyricsLine v
                          cps = case firstLy of
                                 Nothing -> []
                                 Just fl -> filter (`isPrefixOf` fl) chorusPrefixes
                          bps = case firstLy of
                                 Nothing -> []
                                 Just fl -> filter (`isPrefixOf` fl) bridgePrefixes
                      in if cps /= []
                         then Verse Chorus $
                              stripPrefixFromLyricsLine (head cps) v
                         else if bps /= []
                              then Verse Bridge $
                                   stripPrefixFromLyricsLine (head bps) v
                              else if songNumbering s == Numbered
                                   then Verse NormalVerse v
                                   else Verse Bridge v
        chorusPrefixes = ["Refrain:", "Refrain", "Refr.:", "Refr.", "Refr",
                          "Ref.:", "Ref.", "Ref ", "R.:", "Chorus:", "Chorus",
                          "Chor.:", "Chor.", "Chor "]
        bridgePrefixes = ["Bridge:", "Bridge", "Br.:", "Br.", "Br "]
        isEndLine      = (== "***") . take 3 . dropWhile isSpace
        isLyricsLine l = not (isChordsLine l || isNotesLine l)
        firstLyricsLine v = case filter isLyricsLine v of
                             [] -> Nothing
                             v' -> Just . head $ v'
        stripPrefixFromLyricsLine :: String -> [Line] -> [Line]
        stripPrefixFromLyricsLine p v =
          case break isLyricsLine v of
           (v, []) -> v
           (pref, l:rest) ->
             let l' = dropWhile isSpace . fromJust . stripPrefix p $ l
             in map (drop (length l - length l')) pref ++ l' : rest

