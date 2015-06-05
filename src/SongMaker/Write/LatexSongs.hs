module SongMaker.Write.LatexSongs
  ( insertChords
  , writeHeader
  , writeFooter) where

import SongMaker.Common

import Data.List
import Data.Char

insertChords :: ChordIndexes -> Line -> Line
insertChords cs l = go (reverse cs) l
  where toChord c = "\\[" ++ c ++ "]"
        go [] l = l
        go ((i,c):cs) l = let l' = if length l < i
                                   then l ++ replicate (i - length l) ' '
                                   else l
                              (before, after) = splitAt i l'
                          in go cs (before ++ toChord c ++ after)


get k = filter ((==k).fst)

writeHeader :: Song -> Stream
writeHeader s = "\\beginsong{"++songTitle s++"}["++other++"]"
  where other = intercalate "," . filter (not.null) . map toSongs $ others
        toSongs (k,f) = case f s of
          Nothing -> []
          Just v -> k++"={"++v++"}"
        others = [ ("by", makeAuthor)
                 , ("cr", songCopyright)
                 , ("sr", songScriptureRef) ]
        makeAuthorTM s = case (songAuthorLyrics s, songAuthorMusic s) of
                          (Just a, Nothing) -> Just $ "T: " ++ a
                          (Nothing, Just a) -> Just $ "M: " ++ a
                          (Just a, Just b) -> if a `sameAs` b
                                              then Just $ "T/M: " ++ a
                                              else Just $ "T: " ++ a ++ ", M: " ++ b
                          (Nothing, Nothing) -> Nothing
        makeAuthor s = case (makeAuthorTM s, songAuthorTranslation s) of
                        (Just a, Nothing) -> Just a
                        (Nothing, Just a) -> Just $ "D: " ++ a
                        (Just a, Just b) -> Just $ a ++ ", D: " ++ b
                        (Nothing, Nothing) -> Nothing
        sameAs a b = (words $ map toLower a) == (words $ map toLower b)

writeFooter :: Song -> Stream
writeFooter _ = "\\endsong"
