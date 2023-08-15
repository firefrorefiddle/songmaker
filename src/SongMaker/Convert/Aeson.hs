module SongMaker.Convert.Aeson where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 (unpack)
import SongMaker.Convert.Stream
import SongMaker.Format.Aeson

instance SongOutput JsonStream where
    convertSong = liftJson . unpack . encodePretty
