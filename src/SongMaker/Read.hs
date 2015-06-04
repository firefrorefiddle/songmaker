module SongMaker.Read 
   ( module SongMaker.Read.Chord
   , module SongMaker.Read.Song
   , readStream
   ) where

import SongMaker.Read.Chord
import SongMaker.Read.Song
import SongMaker.Read.Header
import SongMaker.Common

readStream :: Stream -> (Header, [Line])
readStream s = (readHeader s, skipHeader s ++ [""])

