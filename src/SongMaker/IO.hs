module SongMaker.IO (run,
                     actDirectory,
                     actFile,
                     actFilePath,
                     convertStringJson,
                     convertStringLatex) where

import SongMaker.Convert.LatexSongs
import SongMaker.Format.LatexSongs
import SongMaker.Convert.Aeson
import SongMaker.Format.Aeson
import SongMaker.Convert.Stream
import SongMaker.Common

import System.Environment (getProgName, getArgs)
import System.Directory
import System.FilePath (splitExtension, takeExtension, (</>))
import System.IO
import Control.Exception
import Control.Applicative
import Control.Monad.Reader

data Settings = Settings
                { getConverter :: Stream -> Either String Stream
                , getExtension :: String
                }

latexSettings = Settings
                (\s -> let ls :: Either String LatexStream
                           ls = convertStream s
                       in toStream <$> ls)
                ".tex"

jsonSettings = Settings
               (\s -> let ls :: Either String JsonStream
                          ls = convertStream s
                      in toStream <$> ls)
               ".json"

type Prog = ReaderT Settings IO

convertStringLatex :: String -> Either String String
convertStringLatex = getConverter latexSettings

convertStringJson :: String -> Either String String
convertStringJson = getConverter jsonSettings

run :: IO ()
run = do
  args <- getArgs
  prog <- getProgName
  case args of
    []         -> interact (either error id . convertStringLatex)
    ["-j"]     -> interact (either error id . convertStringJson)
    [fp]       -> runReaderT (actFilePath fp) latexSettings
    ["-j", fp] -> runReaderT (actFilePath fp) jsonSettings
    [fp, "-j"] -> runReaderT (actFilePath fp) jsonSettings
    _ -> error $ "Usage: "++prog++" [-j] [filename]"

actDirectory :: FilePath -> Prog ()
actDirectory fp = do
  cont <- lift $ getDirectoryContents fp
  let cont' = map (fp </>) .
              filter ((== ".sng") . takeExtension) $
              cont  
  mapM_ (\fp -> catch' (actFile fp) (lift . printIOException)) cont'

printIOException :: IOException -> IO ()
printIOException = hPrint stderr

--- I am truly left banging my head for the need of this and the next function...
catch' :: (Exception e) => Prog a -> (e -> Prog a) -> Prog a
catch' f h = do settings <- ask
                lift $ catch (runReaderT f settings) (\e -> runReaderT (h e) settings)

withFile' :: FilePath -> IOMode -> (Handle -> Prog r) -> Prog r
withFile' fp m f = do settings <- ask
                      lift $ withFile fp m (\h -> runReaderT (f h) settings)

actFile :: FilePath -> Prog ()
actFile fp = do
  ext <- getExtension <$> ask
  case splitExtension fp of
    (base, ".sng") -> withFile' fp ReadMode $ \inh ->
                      withFile' (base ++ ext) WriteMode $ \outh -> do
                         lift $ hSetEncoding inh utf8
                         lift $ hSetEncoding outh utf8
                         contents <- lift $ hGetContents inh
                         conv <- getConverter <$> ask
                         case conv contents of
                          Left err -> error err
                          Right c' -> lift $ hPutStr outh c'
    _ -> error $ "Filepath must have .sng extension: " ++ fp

actFilePath :: FilePath -> Prog ()
actFilePath fp = do
  dir <- lift $ doesDirectoryExist fp
  if dir
    then actDirectory fp
    else do
      file <- lift $ doesFileExist fp
      if file
        then actFile fp
        else error $ "File or Directory not found: " ++ fp
