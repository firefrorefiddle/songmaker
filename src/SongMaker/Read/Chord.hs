module SongMaker.Read.Chord (isChord, isChordsLine, chordsFromLine) where

import SongMaker.Common

import Data.List
import Data.Char

isChord :: Word -> Bool
isChord = isChord' . removeSuffixes . map toLower

isChordsLine :: Line -> Bool
isChordsLine = (>0.8) . truePart . map isChord . words . removeSpecials
  where truePart xs = let trues = fromIntegral $ length . filter (==True) $ xs
                          falses = fromIntegral $ length . filter (==False) $ xs
                      in trues / falses
        removeSpecials = replace (`elem` ['(', ')', '/']) ' '

chordsFromLine :: Line -> [(Int, Word)]
chordsFromLine l =
  let idxd = zip [0..] l
  in scan idxd
  where isWord = not.isSpace.snd
        ret w = (fst.head $ w, map snd w)
        scan il =
          case dropWhile (not.isWord) il of
            [] -> []
            xs -> (ret.takeWord $ xs) : (scan.dropWord $ xs)
        takeWord = takeWhile isWord
        dropWord = dropWhile isWord


baseChords = (map.map) toLower ["C","D","E","F","G","A","H","B"]
isBaseChord = (`elem` baseChords)

isChord' w = isBaseChord w
             || isEnglishFlatChord w
             || isSharpChord w
             || isIsChord w
             || isEsChord w

stripPrefix' :: (Eq a) => [a] -> [a] -> [a]
stripPrefix' p w = maybe w id (stripPrefix p w)

stripSuffix s = reverse . stripPrefix' (reverse s) . reverse

isEnglishFlatChord = isBaseChord . stripSuffix "b"
isSharpChord = isBaseChord . stripSuffix "#"
isIsChord = isBaseChord . stripSuffix "is"
isEsChord = isBaseChord . stripSuffix "es"

removeSuffixes :: String -> String
removeSuffixes w =
  let suffixes = ["maj","min","4","6","7"]
  in foldr ($) w (map stripSuffix suffixes)

replace _ _ [] = []
replace p r (x:xs) | p x       = r : replace p r xs
                   | otherwise = x : replace p r xs

