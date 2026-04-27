module Main where

import Data.List (isInfixOf)
import qualified Data.Text as T
import SongMaker.AST.Parsed
import SongMaker.AST.Semantic
import SongMaker.Convert.Stream (IsStream (toStream))
import SongMaker.Convert.LatexSongs
import SongMaker.Normalize.Semantic (normalizeSong)
import SongMaker.Parse.ParsedSong (parseSongParsed)
import Test.HUnit

main :: IO ()
main = do
    counts <- runTestTT tests
    if failures counts > 0 || errors counts > 0
        then fail "test failures"
        else pure ()

tests :: Test
tests =
    TestList
        [ TestLabel "chord-line parsing" testChordLineParsing
        , TestLabel "caret expansion" testCaretExpansion
        , TestLabel "repeat extraction" testRepeatExtraction
        , TestLabel "section classification" testSectionClassification
        , TestLabel "note/raw block handling" testNoteAndRaw
        , TestLabel "dual backend rendering" testDualBackends
        ]

mustParse :: String -> SongParsed
mustParse s = case parseSongParsed s of
    Left e -> error e
    Right x -> x

mustNormalize :: String -> SongSemantic
mustNormalize = normalizeSong . mustParse

firstSection :: SongSemantic -> SectionSemantic
firstSection s = case [sec | ESection sec <- ssElements s] of
    x : _ -> x
    _ -> error "expected section"

testChordLineParsing :: Test
testChordLineParsing = TestCase $ do
    let s =
            unlines
                [ "title: T"
                , "***"
                , "C     G"
                , "alpha beta"
                ]
        sec = firstSection (mustNormalize s)
        line1 = head (sectionLines sec)
    assertEqual "anchor count" 2 (length (mlAnchors line1))
    assertEqual "first chord" (ChordSymbol (T.pack "C")) (caChord (head (mlAnchors line1)))

testCaretExpansion :: Test
testCaretExpansion = TestCase $ do
    let s =
            unlines
                [ "title: T"
                , "***"
                , "C    G"
                , "line one"
                , ""
                , "^^"
                ]
        secs = [sec | ESection sec <- ssElements (mustNormalize s)]
        secondLine = head (sectionLines (secs !! 1))
    assertEqual "caret creates anchors" 2 (length (mlAnchors secondLine))
    assertEqual "caret removed from lyric" "" (T.unpack (mlLyric secondLine))

testRepeatExtraction :: Test
testRepeatExtraction = TestCase $ do
    let s =
            unlines
                [ "title: T"
                , "***"
                , "C"
                , "|: hello :|"
                ]
        line1 = head . sectionLines . firstSection $ mustNormalize s
    assertBool "repeat open" (mlRepeatOpen line1)
    assertBool "repeat close" (mlRepeatClose line1)
    assertEqual "repeat markers removed" "hello" (T.unpack (mlLyric line1))

testSectionClassification :: Test
testSectionClassification = TestCase $ do
    let s =
            unlines
                [ "title: T"
                , "***"
                , "Ref.:"
                , "line"
                ]
        sec = firstSection (mustNormalize s)
    assertEqual "chorus classification" ChorusSection (sectionKind sec)

testNoteAndRaw :: Test
testNoteAndRaw = TestCase $ do
    let s =
            unlines
                [ "title: T"
                , "unknownField: abc"
                , "***"
                , "!text: remark"
                , ""
                , "\\custom{raw}"
                ]
        parsed = mustParse s
    assertEqual "unknown header preserved" 1 (length (smUnknownFields (spMeta parsed)))
    assertBool "has note block" (any isNote (spBlocks parsed))
    assertBool "has raw tex block" (any isRaw (spBlocks parsed))
  where
    isNote b = case b of
        BPNote _ -> True
        _ -> False
    isRaw b = case b of
        BPRawTeX _ -> True
        _ -> False

testDualBackends :: Test
testDualBackends = TestCase $ do
    let s =
            unlines
                [ "title: T"
                , "***"
                , "C"
                , "line"
                ]
    project <- case convertLatexFromSource ProjectOwnedTeX s of
        Left e -> assertFailure e >> pure ""
        Right ls -> pure (toStream ls)
    sty <- case convertLatexFromSource SongsStyTeX s of
        Left e -> assertFailure e >> pure ""
        Right ls -> pure (toStream ls)
    assertBool "project backend avoids beginsong" (not ("\\beginsong" `isInfixOf` project))
    assertBool "songs.sty backend contains beginsong" ("\\beginsong" `isInfixOf` sty)
