module SongMaker.Read.Chord (isChord, isChordsLine, chordsFromLine) where

import SongMaker.Common

import Data.Char
import Data.List
import Data.Maybe
import Data.Ratio
import Prelude hiding (Word)

isChord :: Word -> Bool
isChord = isChord' . removeSuffixes . map toLower

isChordsLine :: Line -> Bool
isChordsLine = (>= 8 % 10) . truePart . map isChord . words . removeSpecials
  where
    truePart xs =
        let trues = length . filter (== True) $ xs
            falses = length . filter (== False) $ xs
         in if falses == 0
                then 1
                else trues % falses
    removeSpecials = replace (`elem` ['(', ')', '/']) ' '

chordsFromLine :: Line -> [(Int, Word)]
chordsFromLine l =
    let idxd = zip [0 ..] l
     in scan idxd
  where
    isWord = not . isSpace . snd
    ret w = (fst . head $ w, map snd w)
    scan il =
        case dropWhile (not . isWord) il of
            [] -> []
            xs -> (ret . takeWord $ xs) : (scan . dropWord $ xs)
    takeWord = takeWhile isWord
    dropWord = dropWhile isWord

baseChords = (map . map) toLower ["C", "D", "E", "F", "G", "A", "H", "B"]
isBaseChord = (`elem` baseChords)

isChord' w =
    isBaseChord w
        || isEnglishFlatChord w
        || isSharpChord w
        || isIsChord w
        || isEsChord w

stripPrefix' :: (Eq a) => [a] -> [a] -> [a]
stripPrefix' p w = fromMaybe w (stripPrefix p w)

stripSuffix s = reverse . stripPrefix' (reverse s) . reverse

isEnglishFlatChord = isBaseChord . stripSuffix "b"
isSharpChord = isBaseChord . stripSuffix "#"
isIsChord = isBaseChord . stripSuffix "is"
isEsChord = isBaseChord . stripSuffix "es"

removeSuffixes :: String -> String
removeSuffixes w =
    let suffixes = ["maj", "min", "sus", "4", "6", "7"]
     in foldr stripSuffix w suffixes

replace _ _ [] = []
replace p r (x : xs)
    | p x = r : replace p r xs
    | otherwise = x : replace p r xs
