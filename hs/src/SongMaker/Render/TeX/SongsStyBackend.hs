module SongMaker.Render.TeX.SongsStyBackend (renderSongsStyTeX) where

import Data.List (intercalate, sortOn)
import qualified Data.Text as T
import SongMaker.AST.Layout
import SongMaker.AST.Semantic (ChordAnchor (..), ChordSymbol (..), SectionKind (..))

renderSongsStyTeX :: SongLayout -> String
renderSongsStyTeX sl =
    unlines $
        [ "\\beginsong{" ++ esc (T.unpack (lmTitle (slMeta sl))) ++ "}[]"
        ]
            ++ concatMap renderBlock (slBlocks sl)
            ++ ["\\endsong"]

renderBlock :: LayoutBlock -> [String]
renderBlock b = case b of
    LBSection sec ->
        [beginSection (lsRole sec)]
            ++ map renderLine (lsLines sec)
            ++ [endSection (lsRole sec), ""]
    LBNote _ -> []
    LBSpacer _ -> [""]
    LBInterSong _ -> []
    LBRawTeX t -> [T.unpack t]

beginSection :: SectionKind -> String
beginSection k = case k of
    ChorusSection -> "\\beginchorus"
    BridgeSection -> "\\beginverse*"
    _ -> "\\beginverse"

endSection :: SectionKind -> String
endSection ChorusSection = "\\endchorus"
endSection _ = "\\endverse"

renderLine :: LayoutLine -> String
renderLine l = withRepeat (insertAnchors (sortOn caOffset (llChordAnchors l)) (T.unpack (llLyricText l)))
  where
    withRepeat txt =
        let left = if rmOpen (llRepeatMarkers l) then "\\lrep " else ""
            right = if rmClose (llRepeatMarkers l) then " \\rrep" else ""
         in left ++ txt ++ right ++ "\\\\%"

insertAnchors :: [ChordAnchor] -> String -> String
insertAnchors as lyric = foldr put lyric as
  where
    put (ChordAnchor i (ChordSymbol c)) acc =
        let (a, b) = splitAt (max 0 i) (padRight i acc)
         in a ++ "\\[" ++ esc (T.unpack c) ++ "]" ++ b
    padRight i txt =
        if length txt >= i
            then txt
            else txt ++ replicate (i - length txt) ' '

esc :: String -> String
esc = concatMap go
  where
    go '\\' = "\\textbackslash{}"
    go '{' = "\\{"
    go '}' = "\\}"
    go '#' = "\\#"
    go '$' = "\\$"
    go '%' = "\\%"
    go '&' = "\\&"
    go '_' = "\\_"
    go '~' = "\\textasciitilde{}"
    go '^' = "\\textasciicircum{}"
    go c = [c]
