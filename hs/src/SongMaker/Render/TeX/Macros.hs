module SongMaker.Render.TeX.Macros (projectMacros) where

projectMacros :: String
projectMacros =
    unlines
        [ "\\newcommand{\\SongTitle}[1]{\\section*{#1}}"
        , "\\newcommand{\\SongHeaderLine}[1]{\\textit{#1}\\\\}"
        , "\\newenvironment{songsection}[2]{\\par\\medskip\\noindent\\textbf{#2}\\par}{\\par\\medskip}"
        , "\\newcommand{\\SectionLabel}[1]{\\textbf{#1}}"
        , "\\newcommand{\\ChordAt}[2]{\\texttt{(#1:#2)} }"
        , "\\newcommand{\\SongLine}[3]{#1\\\\#2\\\\}"
        , "\\newcommand{\\SongTextNote}[1]{\\emph{#1}\\par}"
        , "\\newcommand{\\SongMusicNote}[1]{\\emph{#1}\\par}"
        ]
