module SongMaker.Convert.Stream (
    IsStream,
    toStream,
    fromStream,
    (+++),
    sconcat,
    SongOutput,
    convertSong,
    convertStream,
) where

import Control.Applicative
import SongMaker.Common
import SongMaker.Read

class IsStream s where
    toStream :: s -> Stream
    fromStream :: Stream -> s
    (+++) :: s -> s -> s
    s1 +++ s2 = fromStream $ toStream s1 ++ toStream s2
    sconcat :: [s] -> s
    sconcat = foldr (+++) (fromStream "")

class (IsStream s) => SongOutput s where
    convertSong :: Song -> s

convertStream :: (SongOutput s) => Stream -> Either String s
convertStream s = convertSong <$> readStream s
