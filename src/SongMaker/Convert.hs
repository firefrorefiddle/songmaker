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
import Control.Applicative

processSpecialChars = replaceSubStr "\t" "    " .
                      replaceSubStr "$" "\\brk"

convertStream :: Stream -> Either String Stream
convertStream s = do
  song <- readStream s
  return $
    writeHeader song ++
    unlines (concatMap convertVerse (songVerses song)) ++
    writeFooter song ++
    songAfter song
  
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
conversions = [ processChords
              , always processSpecialChars
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

convertVerse :: Verse -> [Line]
convertVerse (Verse t lyrics) = case t of
                                 Chorus -> "\\beginchorus" : convertLines lyrics ++
                                           ["\\endchorus"]
                                 Bridge -> "\\beginverse*" : convertLines lyrics ++
                                           ["\\endverse"]
                                 NormalVerse -> "\\beginverse" : convertLines lyrics ++
                                                ["\\endverse"]
                                

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

