module SongMaker.Layout.Build (buildLayout) where

import Control.Applicative ((<|>))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import SongMaker.AST.Layout
import SongMaker.AST.Semantic

buildLayout :: SongSemantic -> SongLayout
buildLayout s =
    SongLayout
        { slMeta = buildMeta (ssMeta s)
        , slBlocks = map buildElement (ssElements s)
        }

buildMeta :: SongMetaSemantic -> LayoutMeta
buildMeta m =
    LayoutMeta
        { lmTitle = sTitle m
        , lmSongNumber = Just (T.pack "1")
        , lmHeaderLines = catMaybes [sReference m, sCopyright m, sLicense m]
        , lmReference = sReference m
        , lmAuthorLine = mkAuthorLine (sAuthors m)
        , lmCopyrightLine = sCopyright m <|> sLicense m
        , lmIndexKeys = sIndexes m
        , lmFlags = LayoutFlags {lfNumbered = sNumbering m == NumberedSemantic}
        }

buildElement :: SongElement -> LayoutBlock
buildElement e = case e of
    ESection s ->
        LBSection $
            LayoutSection
                { lsRole = sectionKind s
                , lsVisibleLabel = sectionLabel s
                , lsOrdinal = sectionOrdinal s
                , lsKeepTogether = KeepSection
                , lsLines = map buildLine (sectionLines s)
                , lsEstimatedHeight = Nothing
                }
    ENote n -> LBNote (buildNote n)
    EInterSong t -> LBInterSong t
    ERawTeX t -> LBRawTeX t

buildLine :: MusicalLine -> LayoutLine
buildLine l =
    LayoutLine
        { llLyricText = mlLyric l
        , llChordAnchors = mlAnchors l
        , llRepeatMarkers = RepeatMarkers (mlRepeatOpen l) (mlRepeatClose l)
        , llWrapPolicy = NoAutoWrap
        }

buildNote :: NoteSemantic -> LayoutNote
buildNote n = case n of
    STextNote t -> LTextNote t
    SMusicNote t -> LMusicNote t
    SGenericNote t -> LGenericNote t

mkAuthorLine :: AuthorInfo -> Maybe T.Text
mkAuthorLine a = case (saLyricsBy a <|> saAuthor a, saMusicBy a <|> saAuthor a) of
    (Just t, Just m)
        | T.toCaseFold t == T.toCaseFold m -> Just ("T/M: " <> t)
        | otherwise -> Just ("T: " <> t <> ", M: " <> m)
    (Just t, Nothing) -> Just ("T: " <> t)
    (Nothing, Just m) -> Just ("M: " <> m)
    (Nothing, Nothing) -> Nothing
