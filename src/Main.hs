module Main where

import           Config
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS
import           PsvrTranscoder            (transcode)
import           System.Directory
import           TranscodeJob

main :: IO ()
main = decideWhatToDo =<< parseConfig

decideWhatToDo :: Config -> IO ()
decideWhatToDo Config { monitor = True } = putStrLn "Monitor!"
decideWhatToDo config' = transcodeFilesFromConfig config'

transcodeFilesFromConfig :: Config -> IO ()
transcodeFilesFromConfig config @ Config { paths = paths' } =
  case transcodeJobFromConfig config of
    Just job -> transcodeAll paths' job
    Nothing -> return ()

transcodeAll :: [String] -> TranscodeJob -> IO ()
transcodeAll [] _ = return ()
transcodeAll (path:paths) job = do
  transcodeFileOrDirectory path job
  transcodeAll paths job

transcodeFileOrDirectory :: String -> TranscodeJob -> IO ()
transcodeFileOrDirectory path job = do
  exists <- doesFileExist path
  if exists then transcodeFileIfVideo path job
  else           transcodeDirectoryContents path job

transcodeFileIfVideo :: String -> TranscodeJob -> IO ()
transcodeFileIfVideo path job =
  case maybe_ext of
    Just ext
       | ext == T.pack "mp4" -> transcode path job
       | otherwise -> return ()
    _ -> return ()
  where
    input_file     = decodeString path
    (_, maybe_ext) = splitExtension input_file

transcodeDirectoryContents :: String -> TranscodeJob -> IO ()
transcodeDirectoryContents path job = do
  putStrLn "It's a directory!"
  listDirectory path >>= flip transcodeAll job
