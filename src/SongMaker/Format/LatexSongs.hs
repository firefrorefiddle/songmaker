module SongMaker.Format.LatexSongs
       ( LatexStream
       , liftLatex) where

import SongMaker.Convert.Stream
import SongMaker.Common

newtype LatexStream = LS Stream

liftLatex :: Stream -> LatexStream
liftLatex = LS

instance IsStream LatexStream where
  toStream (LS s) = s
  fromStream = liftLatex
