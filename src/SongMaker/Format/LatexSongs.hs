module SongMaker.Format.LatexSongs (
    LatexStream,
    liftLatex,
) where

import SongMaker.Common
import SongMaker.Convert.Stream

newtype LatexStream = LS Stream

liftLatex :: Stream -> LatexStream
liftLatex = LS

instance IsStream LatexStream where
    toStream (LS s) = s
    fromStream = liftLatex
