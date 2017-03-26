module Main where

import           Config
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS
import           PsvrTranscoder            (transcode)
import           System.Directory

main :: IO ()
main = decideWhatToDo =<< parseConfig

decideWhatToDo :: Config -> IO ()
decideWhatToDo Config { monitor = True } = putStrLn "Monitor!"
decideWhatToDo config' = transcodeFilesFromConfig config'

transcodeFilesFromConfig :: Config -> IO ()
transcodeFilesFromConfig = transcodeAll . paths

transcodeAll :: [String] -> IO ()
transcodeAll [] = return ()
transcodeAll (path:paths) = do
  transcodeFileOrDirectory path
  transcodeAll paths

transcodeFileOrDirectory :: String -> IO ()
transcodeFileOrDirectory path = do
  exists <- doesFileExist path
  if exists then transcodeFileIfVideo path
  else           transcodeDirectoryContents path

transcodeFileIfVideo :: String -> IO ()
transcodeFileIfVideo path =
  case maybe_ext of
    Just ext
       | ext == T.pack "mp4" -> transcode path
       | otherwise -> return ()
    _ -> return ()
  where
    input_file     = decodeString path
    (_, maybe_ext) = splitExtension input_file

transcodeDirectoryContents :: String -> IO ()
transcodeDirectoryContents path = do
  putStrLn "It's a directory!"
  transcodeAll =<< listDirectory path
