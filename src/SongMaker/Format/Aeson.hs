module SongMaker.Format.Aeson
       ( JsonStream
       , liftJson) where

import GHC.Generics
import Data.Aeson

import SongMaker.Convert.Stream
import SongMaker.Common
import SongMaker.Common.Song

newtype JsonStream = JS Stream

liftJson :: Stream -> JsonStream
liftJson = JS

instance IsStream JsonStream where
  toStream (JS s) = s
  fromStream = liftJson

instance ToJSON Song where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Song
instance ToJSON SongNumbering where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON SongNumbering
instance ToJSON VerseType where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON VerseType
instance ToJSON Verse where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Verse
