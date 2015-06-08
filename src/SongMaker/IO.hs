module SongMaker.IO (run,
                     actDirectory,
                     actFile,
                     actFilePath) where

import SongMaker.Convert

import System.Environment (getArgs)
import System.Directory
import System.FilePath (splitExtension, takeExtension, (</>))
import System.IO
import Control.Exception

run :: IO ()
run = do
  args <- getArgs
  case args of
    [] -> interact (either error id . convertStream)
    [fp] -> actFilePath fp
    _ -> error "Please give exactly zero or one argument (File or Directory)."

actDirectory fp = do
  cont <- getDirectoryContents fp
  let cont' = map (fp </>) .
              filter ((== ".sng") . takeExtension) $
              cont  
  mapM_ (\fp -> catch (actFile fp) printIOException) cont'

printIOException :: IOException -> IO ()
printIOException = hPutStrLn stderr . show
  
actFile fp =
  case splitExtension fp of
    (base, ".sng") -> withFile fp ReadMode $ \inh -> 
                      withFile (base ++ ".tex") WriteMode $ \outh -> do
                         hSetEncoding inh utf8
                         hSetEncoding outh utf8
                         contents <- hGetContents inh
                         case convertStream contents of
                          Left err -> error err
                          Right c' -> hPutStr outh c'
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
