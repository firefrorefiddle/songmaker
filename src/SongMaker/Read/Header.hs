module SongMaker.Read.Header where

import SongMaker.Common
import Data.Maybe
import Data.List

knownHeaders = ["title",
                "author",
                "copyright",
                "reference",
                "output",
                "license",
                "extra-index",
                "columns",
                "extra-title-index"]

splitEach :: (Eq a) => a -> [a] -> [[a]]
splitEach c l = case break (== c) l of
  (l, []) -> [l]
  (l, r) -> l:splitEach c (dropWhile (== c) r)

tryReadHeaderLine l = case splitEach ':' l of 
  [name, value] ->
    let value' = stripPrefix ":" value
    in if length (words name) == 1
       then return (head.words $ name, value)
       else fail "invalid header format"
  _ -> fail "invalid header format"
                        
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
