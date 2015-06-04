module SongMaker.Common where

import Data.List

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
