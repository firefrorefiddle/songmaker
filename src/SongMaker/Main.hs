module SongMaker.Main (run) where

import SongMaker.Convert

import System.Environment (getArgs)
import System.Directory
import System.FilePath (splitExtension, takeExtension, (</>))
import System.IO

run :: IO ()
run = do
  args <- getArgs
  case args of
    [] -> interact convertStream
    [fp] -> actFilePath fp
    _ -> error "Please give exactly zero or one argument (File or Directory)."

actDirectory fp = do
  cont <- getDirectoryContents fp
  let cont' = map (fp </>) .
              filter ((== ".sng") . takeExtension) $
              cont  
  mapM_ actFile cont'
  
actFile fp =
  case splitExtension fp of
    (base, ".sng") -> withFile fp ReadMode $ \inh -> 
                      withFile (base ++ ".tex") WriteMode $ \outh -> do
                         hSetEncoding inh utf8
                         hSetEncoding outh utf8
                         contents <- hGetContents inh
                         hPutStr outh . convertStream $ contents
    _ -> error $ "Filepath must have .sng extension: " ++ fp

actFilePath fp = do
  dir <- doesDirectoryExist fp
  if dir
    then actDirectory fp
    else do
      file <- doesFileExist fp
      if file
        then actFile fp
        else error $ "File or Directory not found: " ++ fp
