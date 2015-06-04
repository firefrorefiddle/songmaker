module SongMaker.Convert.Misc
       (replaceUnderscores) where

import SongMaker.Common
import Data.List

replaceUnderscores :: Line -> Line
replaceUnderscores l | "_" `isInfixOf` l =
                         let (front, rest) = span (/= '_') l
                             (uscs, end) = span (== '_') rest
                             len = 0.5 * fromIntegral (length uscs)
                         in replaceUnderscores $
                            front ++ "\\underline{\\hspace{" ++ show len ++ "cm}}" ++ end
  
                     | otherwise = l
