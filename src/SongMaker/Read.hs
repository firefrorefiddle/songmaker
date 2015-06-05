module SongMaker.Read 
   ( module SongMaker.Read.Chord
   , module SongMaker.Read.Song
   , readStream
   ) where

import SongMaker.Read.Chord
import SongMaker.Read.Song
import SongMaker.Read.Header
import SongMaker.Common

import Control.Applicative
import Data.List
import Data.Char
import Data.Maybe

readStream :: Stream -> Either String Song
readStream s =
  let header = readHeader s
      (verseLines, afterSong) = break isEndLine . skipHeader $ s
      rawVerses = filter ((nall isSpace) . concat) .
                  groupBy (\x y -> all  isSpace x && all  isSpace y ||                                   nall isSpace x && nall isSpace y) $ verseLines
  in do
    title <- case lookup "title" header of
                 Just title -> return title
                 Nothing    -> Left "song has no title on songmaker import"
    let ret = Song
           { songTitle             = title 
           , songAuthorLyrics      = lookup "lyricsBy" header
           , songAuthorMusic       = lookup "musicBy" header
           , songAuthorTranslation = lookup "translationBy" header
           , songCopyright         = lookup "copyright" header
           , songScriptureRef      = lookup "reference" header
           , songKey               = lookup "key" header
           , songNumbering = case lookup "numbering" header of
                              Nothing -> Numbered
                              Just "numbered" -> Numbered
                              Just "chorus" -> ChorusOnly
                              Just "unnumbered" -> Unnumbered
                              Just other -> warn ("ignoring unknown numbering " ++ other) $
                                            Numbered
           , songVerses = map (mkVerse ret) rawVerses
           , songAfter = if null afterSong
                         then ""
                         else unlines (tail afterSong)}
    return ret
  where nall p = not . all p
        mkVerse s v = let firstLy = firstLyricsLine v
                          cps = case firstLy of
                                 Nothing -> []
                                 Just fl -> filter (`isPrefixOf` fl) $
                                            chorusPrefixes
                          bps = case firstLy of
                                 Nothing -> []
                                 Just fl -> filter (`isPrefixOf` fl) $
                                            bridgePrefixes
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
        isLyricsLine   = not . isChordsLine
        firstLyricsLine v = case filter isLyricsLine v of
                             [] -> Nothing
                             v' -> Just . head $ v'
        stripPrefixFromLyricsLine :: String -> [Line] -> [Line]
        stripPrefixFromLyricsLine p v =
          case break isLyricsLine v of
           (v, []) -> v
           (pref, (l:rest)) ->
             let l' = dropWhile isSpace . fromJust . stripPrefix p $ l
             in map (drop (length l - length l')) pref ++ l' : rest

