module SongMaker.AST.Semantic where

import Data.Text (Text)

data SongSemantic = SongSemantic
    { ssMeta :: SongMetaSemantic
    , ssElements :: [SongElement]
    , ssWarnings :: [SongWarning]
    }
    deriving (Show, Eq)

data SongNumberingSemantic
    = NumberedSemantic
    | ChorusOnlySemantic
    | UnnumberedSemantic
    deriving (Show, Eq)

data AuthorInfo = AuthorInfo
    { saAuthor :: Maybe Text
    , saLyricsBy :: Maybe Text
    , saMusicBy :: Maybe Text
    , saTranslationBy :: Maybe Text
    , saOriginalTitle :: Maybe Text
    }
    deriving (Show, Eq)

data SongMetaSemantic = SongMetaSemantic
    { sTitle :: Text
    , sSubtitle :: Maybe Text
    , sAuthors :: AuthorInfo
    , sCopyright :: Maybe Text
    , sReference :: Maybe Text
    , sLicense :: Maybe Text
    , sIndexes :: [Text]
    , sNumbering :: SongNumberingSemantic
    }
    deriving (Show, Eq)

data SongElement
    = ESection SectionSemantic
    | ENote NoteSemantic
    | EInterSong Text
    | ERawTeX Text
    deriving (Show, Eq)

data NoteSemantic
    = STextNote Text
    | SMusicNote Text
    | SGenericNote Text
    deriving (Show, Eq)

data SectionKind
    = VerseSection
    | ChorusSection
    | BridgeSection
    | IntroSection
    | OutroSection
    | PreChorusSection
    | TagSection
    | UnknownSection
    deriving (Show, Eq)

data SectionSemantic = SectionSemantic
    { sectionKind :: SectionKind
    , sectionLabel :: Maybe Text
    , sectionOrdinal :: Maybe Int
    , sectionLines :: [MusicalLine]
    , sectionRepeatStyle :: Maybe RepeatStyle
    }
    deriving (Show, Eq)

data MusicalLine = MusicalLine
    { mlLyric :: Text
    , mlAnchors :: [ChordAnchor]
    , mlRepeatOpen :: Bool
    , mlRepeatClose :: Bool
    , mlSourceComments :: [Text]
    }
    deriving (Show, Eq)

data ChordAnchor = ChordAnchor
    { caOffset :: Int
    , caChord :: ChordSymbol
    }
    deriving (Show, Eq)

newtype ChordSymbol = ChordSymbol Text
    deriving (Show, Eq)

data RepeatStyle = RepeatBar
    deriving (Show, Eq)

data SongWarning
    = UnresolvedCaretPattern Text
    | UnsupportedInlineTeX Text
    | AmbiguousSectionLabel Text
    | InvalidChordLine Text
    | OtherWarning Text
    deriving (Show, Eq)
