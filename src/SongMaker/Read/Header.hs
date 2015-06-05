module SongMaker.Read.Header where

import SongMaker.Common
import Data.Maybe
import Data.List
import Data.Char

knownHeaders = ["title",
                "original",
                "author",
                "lyricsBy",
                "musicBy",
                "translationBy",
                "key",
                "copyright",
                "reference",
                "output",
                "license",
                "extra-index",
                "columns",
                "extra-title-index",
                "numbering"]

{-splitEach :: (Eq a) => a -> [a] -> [[a]]
splitEach c l = case break (== c) l of
  (l, []) -> [l]
  (l, r) -> l:splitEach c (dropWhile (== c) r)
-}

tryReadHeaderLine l =
  let (name, value) = break (==':') l
      value' = dropWhile isSpace . reverse .
               dropWhile (not . isAlphaNum) . reverse .
               dropWhile (== ':') $ value
  in if null value'
     then fail "empty header value"
     else if length (words name) == 1
          then return (head.words $ name, value')
          else fail "invalid header format"
                        
isHeaderLine = isJust . tryReadHeaderLine
readHeaderLine = fromJust . tryReadHeaderLine

isEndOfHeaderMarker = ("***" `isPrefixOf`)

readHeader = filter known .
             catMaybes .
             map tryReadHeaderLine .
             takeWhile (not . isEndOfHeaderMarker) .
             lines
  where known (k,_) = k `elem` knownHeaders

skipHeader = dropWhile isEndOfHeaderMarker .
             dropWhile (not . isEndOfHeaderMarker) .
             lines
