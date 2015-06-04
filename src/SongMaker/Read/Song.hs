module SongMaker.Read.Song (isEndLine) where

import Data.Char

isEndLine l = case dropWhile isSpace l of
               ('*':'*':'*':_) -> True
               _ -> False
