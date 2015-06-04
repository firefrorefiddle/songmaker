module SongMaker.Convert 
  ( convertStream
  , convertLines) where

import SongMaker.Common
import SongMaker.Read
import SongMaker.Write
import SongMaker.Convert.Misc
import Text.Regex.Posix

import Data.Char
import Data.List
import Debug.Trace

processSpecialChars = replaceSubStr "\t" "    " .
                      replaceSubStr "$" "\\brk"

convertStream :: Stream -> Stream
convertStream s = let (h, ls) = readStream s
                  in (writeHeader h) ++
                     "\n\\beginverse\n"++
                     (unlines . convertLines $ ls) ++
                     (writeFooter h)

type Conversion = [Line] -> ([Line], [Line])
type SimpleConversion = Line -> Line

type Matcher = [Line] -> Bool
type SimpleMatcher = Line -> Bool

sMatch :: SimpleMatcher -> Matcher
sMatch _ [] = False
sMatch m (l:_) = m l

sConv :: SimpleConversion -> Conversion
sConv _ [] = ([], [])
sConv c (l:ls) = ([], c l : ls)

smc :: SimpleMatcher -> SimpleConversion -> (Matcher, Conversion)
smc m c = (sMatch m, sConv c)

always :: SimpleConversion -> (Matcher, Conversion)
always m = smc (const True)  m

conversions :: [(Matcher, Conversion)]
conversions = [ dropEmptyEnd
              , processChords
              , always processSpecialChars
              , processEndSong
              , nextVerse
              , always replaceUnderscores
              ]
  where dropEmptyEnd = (all isSpace . concat, const (["\\endverse", "\\endsong"], []))
        processChords = ((\ls -> case ls of
                                 (x:y:xs) -> -- traceShow (x ++ " is a chords line? " ++ show (isChordsLine x)) $
                                             isChordsLine x
                                 _ -> False),
                         (\(x:y:xs) -> ([], insertChords (chordsFromLine x) y : xs)))
        processEndSong = (sMatch isEndLine,
                          (const (["\\endverse", "\\endsong"], [])))
        nextVerse = (sMatch (all isSpace),
                     (\(x:xs) -> let xs' = dropWhile (all isSpace) xs
                                 in case xs' of
                                     [] -> ([], [])
                                     _  -> (["\\endverse", "\\beginverse"], xs')))


applyMC :: (Matcher, Conversion) -> ([Line], [Line]) -> ([Line], [Line])
applyMC (m, c) ([], ys) = if m ys
                          then c ys
                          else ([], ys)
applyMC _ xys = xys

convertLines' :: ([Line], [Line]) -> ([Line], [Line])
convertLines' = flip (foldl' (flip applyMC)) conversions

convertLines :: [Line] -> [Line]
convertLines [] = []
convertLines ls = let (xs, ys) = convertLines' ([], ls)
                  in case xs of
                      [] -> case ys of
                        [] -> []
                        (y:ys') -> y:convertLines ys'
                      _ -> xs ++ convertLines ys

{- convertLines [] = ["\\endverse\\endsong"]
convertLines [x] | all isSpace x = convertLines []
                 | otherwise = x:convertLines []
convertLines (x:y:xs) | all isSpace x = ["\\endverse","","\\beginverse"] ++ convertLines (y:xs)
                      | isChordsLine x = (processSpecialChars .
                                          insertChords (chordsFromLine x) $ y) :
                                         convertLines xs
                      | isEndLine x = ["\\endverse\\endsong"] ++ processRest (y:xs)
                      | otherwise = x : convertLines (y:xs)
-}
-- processRest xs = xs                                    
