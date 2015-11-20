module SongMaker.Convert.Aeson where

import SongMaker.Format.Aeson
import SongMaker.Convert.Stream
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Aeson

instance SongOutput JsonStream where
  convertSong = liftJson . unpack . encode
