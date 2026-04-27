module SongMaker.Convert.LatexSongs (
    TeXBackend (..),
    convertLatexFromSource,
) where

import SongMaker.Format.LatexSongs
import SongMaker.Layout.Build (buildLayout)
import SongMaker.Normalize.Semantic (normalizeSong)
import SongMaker.Parse.ParsedSong (parseSongParsed)
import SongMaker.Render.TeX.LayoutBackend (renderProjectOwnedTeX)
import SongMaker.Render.TeX.SongsStyBackend (renderSongsStyTeX)

data TeXBackend = ProjectOwnedTeX | SongsStyTeX
    deriving (Show, Eq)

convertLatexFromSource :: TeXBackend -> String -> Either String LatexStream
convertLatexFromSource backend src = do
    parsed <- parseSongParsed src
    let semantic = normalizeSong parsed
        layout = buildLayout semantic
        out = case backend of
            ProjectOwnedTeX -> renderProjectOwnedTeX layout
            SongsStyTeX -> renderSongsStyTeX layout
    pure (liftLatex out)
