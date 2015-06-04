module SongMaker.Common
     ( module SongMaker.Common.Song
     , Stream, Word, Line
     , ChordIndex
     , ChordIndexes
     , replaceSubStr
     , warn
     ) where

import Data.List
import SongMaker.Common.Song
import Debug.Trace

warn = trace

type Stream = String
type Word = String
type Line = String

type ChordIndex = (Int, Word)
type ChordIndexes = [ChordIndex]

type Header = [(Word, Word)]

replaceSubStr _    _    []  = []
replaceSubStr sstr repl str =
  if sstr `isPrefixOf` str
  then repl ++ replaceSubStr sstr repl (drop (length sstr) str)
  else head str : replaceSubStr sstr repl (tail str)
