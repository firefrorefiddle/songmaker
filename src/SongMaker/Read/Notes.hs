module SongMaker.Read.Notes where

import SongMaker.Common
import Data.List
import Data.Char

type Note = String

isNotesLine :: Line -> Bool
isNotesLine = ("!" `isPrefixOf`)

notesFromLine :: Line -> [(Int, Note)]
notesFromLine line = let line' = ' ':tail line
                     in go 0 [] (zip line' [0..])
  where go start acc ((c, i):cis)
          | isSpace c && null acc =               go (i+1) []         cis
          | isSpace c             = (start,acc) : go (i+1) []         cis
          | otherwise             =               go start (acc++[c]) cis
        go start acc []
          | null acc              = []
          | otherwise             = [(start, acc)]
