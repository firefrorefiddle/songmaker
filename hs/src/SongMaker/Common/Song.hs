{-# LANGUAGE DeriveGeneric #-}

module SongMaker.Common.Song (
    SongNumbering (..),
    VerseType (..),
    Verse (..),
    Song (..),
) where

import GHC.Generics

data SongNumbering = Numbered | ChorusOnly | Unnumbered
    deriving (Read, Show, Eq, Generic)

data VerseType = NormalVerse | Bridge | Chorus
    deriving (Read, Show, Eq, Generic)

data Verse = Verse
    { verseType :: VerseType
    , verseLyrics :: [String]
    }
    deriving (Read, Show, Eq, Generic)

data Song = Song
    { songTitle :: String
    , songOrigTitle :: Maybe String
    , songAuthorLyrics :: Maybe String
    , songAuthorMusic :: Maybe String
    , songAuthorTranslation :: Maybe String
    , songCopyright :: Maybe String
    , songLicense :: Maybe String
    , songScriptureRef :: Maybe String
    , songKey :: Maybe String
    , songNumbering :: SongNumbering
    , songVerses :: [Verse]
    , songAfter :: String
    , songAltTitles :: [String]
    , songLineIndexes :: [String]
    }
    deriving (Show, Eq, Generic)
