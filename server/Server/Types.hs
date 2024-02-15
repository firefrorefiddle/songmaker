{-# LANGUAGE DeriveAnyClass #-}

module Server.Types where

import Relude

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock

type Author = Text
newtype Username = Username {unUserName :: Text}
    deriving (Read, Show, Eq, Generic, FromJSON, ToJSON)

data Song = Song
    { author :: Author
    , title :: Text
    , content :: Text
    , enteredBy :: Username
    }
    deriving (Read, Show, Eq, Generic, FromJSON, ToJSON)

data SongVersion = SongVersion
    { song :: Song
    , enteredBy :: Username
    , designation :: Text
    , timeStamp :: UTCTime
    , content :: Text
    , userComments :: Text
    }
    deriving (Read, Show, Eq, Generic, FromJSON, ToJSON)

data SongbookContent = SBSong Song | SBNumbering Int
    deriving (Read, Show, Eq, Generic, FromJSON, ToJSON)

newtype SongBook = SongBook
    { title :: Text
    }
    deriving (Read, Show, Eq, Generic, FromJSON, ToJSON)

data SongBookVersion = SongBookVersion
    { songBook :: SongBook
    , designation :: Text
    , enteredBy :: Username
    , contents :: [SongbookContent]
    }
    deriving (Read, Show, Eq, Generic, FromJSON, ToJSON)

data User = User
    { username :: Text
    , firstName :: Text
    , lastName :: Text
    }
    deriving (Read, Show, Eq, Generic, FromJSON, ToJSON)
