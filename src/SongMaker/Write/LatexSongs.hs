module SongMaker.Write.LatexSongs (
    insertChords,
    writeHeader,
    writeFooter,
) where

import SongMaker.Common
import SongMaker.Format.LatexSongs

import Control.Applicative
import Data.Char
import Data.List

insertChords :: ChordIndexes -> Line -> Line
insertChords cs = go (reverse cs)
  where
    toChord c = "\\[" ++ c ++ "]"
    go [] l = l
    go ((i, c) : cs) l =
        let l' =
                if length l < i
                    then l ++ replicate (i - length l) ' '
                    else l
            (before, after) = splitAt i l'
         in go cs (before ++ toChord c ++ after)

get k = filter ((== k) . fst)

writeHeader :: Song -> LatexStream
writeHeader s = liftLatex $ "\\beginsong{" ++ songTitle s ++ "}[" ++ other ++ "]"
  where
    other = intercalate "," . filter (not . null) . map toSongs $ others ++ indexes
    toSongs (k, f) = case f s of
        Nothing -> []
        Just v -> k ++ "={" ++ v ++ "}"
    others =
        [ ("by", makeAuthor)
        , ("cr", songCopyright)
        , ("sr", songScriptureRef)
        , ("li", songLicense)
        ]
    indexes =
        map (\t -> ("ititle", const $ Just t)) (songAltTitles s)
            ++ map (\i -> ("index", const $ Just i)) (songLineIndexes s)
    makeAuthor s =
        concatMaybes
            (\x y -> x ++ ", " ++ y)
            [ makeAuthorTM s
            , ("D: " ++) <$> songAuthorTranslation s
            , ("Orig.: " ++) <$> songOrigTitle s
            ]

    makeAuthorTM s = case (songAuthorLyrics s, songAuthorMusic s) of
        (Just a, Nothing) -> Just $ "T: " ++ a
        (Nothing, Just a) -> Just $ "M: " ++ a
        (Just a, Just b) ->
            if a `sameAs` b
                then Just $ "T/M: " ++ a
                else Just $ "T: " ++ a ++ ", M: " ++ b
        (Nothing, Nothing) -> Nothing

    sameAs a b = words (map toLower a) == words (map toLower b)

concatMaybes f [] = Nothing
concatMaybes f (Nothing : rest) = concatMaybes f rest
concatMaybes f (Just x : rest) = case concatMaybes f rest of
    Nothing -> Just x
    Just y -> Just (x `f` y)

writeFooter :: Song -> LatexStream
writeFooter _ = liftLatex "\\endsong"
