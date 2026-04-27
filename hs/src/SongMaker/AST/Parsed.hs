module SongMaker.AST.Parsed where

import Data.Text (Text)

data SongParsed = SongParsed
    { spMeta :: SongMetaParsed
    , spBlocks :: [BlockParsed]
    }
    deriving (Show, Eq)

data SongNumberingParsed = NumberedParsed | ChorusOnlyParsed | UnnumberedParsed
    deriving (Show, Eq)

data SongMetaParsed = SongMetaParsed
    { smTitle :: Text
    , smOriginal :: Maybe Text
    , smAuthor :: Maybe Text
    , smLyricsBy :: Maybe Text
    , smMusicBy :: Maybe Text
    , smTranslationBy :: Maybe Text
    , smKey :: Maybe Text
    , smCopyright :: Maybe Text
    , smReference :: Maybe Text
    , smLicense :: Maybe Text
    , smNumbering :: Maybe SongNumberingParsed
    , smExtraIndex :: [Text]
    , smExtraTitleIndex :: [Text]
    , smUnknownFields :: [(Text, Text)]
    }
    deriving (Show, Eq)

data BlockParsed
    = BPSection SectionParsed
    | BPNote NoteParsed
    | BPInterSong RawBlock
    | BPRawTeX RawBlock
    deriving (Show, Eq)

data SectionParsed = SectionParsed
    { secParsedLabel :: Maybe Text
    , secParsedLines :: [LineParsed]
    , secParsedSource :: SectionSourceKind
    }
    deriving (Show, Eq)

data SectionSourceKind
    = SourceVerse
    | SourceChorus
    | SourceBridge
    | SourceUnknown
    deriving (Show, Eq)

data LineParsed
    = LPChordLyric
          { lpChordLine :: Text
          , lpLyricLine :: Text
          }
    | LPCaretLyric
          { lpCaretLyric :: Text
          }
    | LPInlineChordLyric
          { lpInlineText :: Text
          }
    | LPRawLine
          { lpRawText :: Text
          }
    deriving (Show, Eq)

data NoteParsed
    = TextNote Text
    | MusicNote Text
    | GenericNote Text
    deriving (Show, Eq)

data RawBlock = RawBlock
    { rawBlockText :: Text
    , rawBlockOrigin :: RawOrigin
    }
    deriving (Show, Eq)

data RawOrigin
    = RawTeXOrigin
    | InterSongOrigin
    | UnknownOrigin
    deriving (Show, Eq)
