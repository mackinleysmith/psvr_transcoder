module Main where

import           Config
import           PsvrTranscoder      (transcode)

main :: IO ()
main = transcodeFilesFromConfig =<< parseConfig

transcodeFilesFromConfig :: Config -> IO ()
transcodeFilesFromConfig = transcodeFiles . files

transcodeFiles :: [String] -> IO ()
transcodeFiles [] = return ()
transcodeFiles (file:files) = do
  transcode file
  transcodeFiles files