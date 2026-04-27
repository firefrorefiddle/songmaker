module SongMaker.AST.Layout where

import Data.Text (Text)
import SongMaker.AST.Semantic (ChordAnchor, SectionKind)

data SongLayout = SongLayout
    { slMeta :: LayoutMeta
    , slBlocks :: [LayoutBlock]
    }
    deriving (Show, Eq)

data LayoutMeta = LayoutMeta
    { lmTitle :: Text
    , lmSongNumber :: Maybe Text
    , lmHeaderLines :: [Text]
    , lmReference :: Maybe Text
    , lmAuthorLine :: Maybe Text
    , lmCopyrightLine :: Maybe Text
    , lmIndexKeys :: [Text]
    , lmFlags :: LayoutFlags
    }
    deriving (Show, Eq)

data LayoutFlags = LayoutFlags
    { lfNumbered :: Bool
    }
    deriving (Show, Eq)

data LayoutBlock
    = LBSection LayoutSection
    | LBNote LayoutNote
    | LBSpacer LayoutSpacer
    | LBInterSong Text
    | LBRawTeX Text
    deriving (Show, Eq)

data LayoutSpacer = LayoutSpacer
    deriving (Show, Eq)

data LayoutNote
    = LTextNote Text
    | LMusicNote Text
    | LGenericNote Text
    deriving (Show, Eq)

data LayoutSection = LayoutSection
    { lsRole :: SectionKind
    , lsVisibleLabel :: Maybe Text
    , lsOrdinal :: Maybe Int
    , lsKeepTogether :: KeepTogether
    , lsLines :: [LayoutLine]
    , lsEstimatedHeight :: Maybe Double
    }
    deriving (Show, Eq)

data KeepTogether
    = KeepSection
    | AllowSplitBetweenLines
    deriving (Show, Eq)

data LayoutLine = LayoutLine
    { llLyricText :: Text
    , llChordAnchors :: [ChordAnchor]
    , llRepeatMarkers :: RepeatMarkers
    , llWrapPolicy :: WrapPolicy
    }
    deriving (Show, Eq)

data RepeatMarkers = RepeatMarkers
    { rmOpen :: Bool
    , rmClose :: Bool
    }
    deriving (Show, Eq)

data WrapPolicy
    = NoAutoWrap
    | ManualSplit [Int]
    deriving (Show, Eq)
