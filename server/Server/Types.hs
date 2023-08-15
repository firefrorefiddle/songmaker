module Server.Types where

import Relude

import Data.Text (Text)

type Author = Text

data Song = Song
    { author :: Author
    , title :: Text
    , content :: Text
    }
    deriving (Read, Show, Eq, Generic)

data SongbookContent = SBSong Song | SBNumbering Int
    deriving (Read, Show, Eq, Generic)

data SongBook = SongBook
    { title :: Text
    , songs :: [Song]
    }
