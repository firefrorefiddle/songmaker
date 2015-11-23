module SongMaker.Convert.Aeson where

import SongMaker.Format.Aeson
import SongMaker.Convert.Stream
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Aeson
import Data.Aeson.Encode.Pretty

instance SongOutput JsonStream where
  convertSong = liftJson . unpack . encodePretty
