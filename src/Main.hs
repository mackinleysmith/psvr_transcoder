module Main where

import           Config
import           PsvrTranscoder   (transcode)
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
  if exists then transcode path
  else transcodeDirectoryContents path

transcodeDirectoryContents :: String -> IO ()
transcodeDirectoryContents path = do
  putStrLn "It's a directory!"
  transcodeAll =<< listDirectory path