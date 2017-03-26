module Main where

import           Config
import           PsvrTranscoder      (transcode)

main :: IO ()
main = decideWhatToDo =<< parseConfig

decideWhatToDo :: Config -> IO ()
decideWhatToDo Config { monitor = True } = putStrLn "Monitor!"
decideWhatToDo config' = transcodeFilesFromConfig config'

transcodeFilesFromConfig :: Config -> IO ()
transcodeFilesFromConfig = transcodeFiles . paths

transcodeFiles :: [String] -> IO ()
transcodeFiles [] = return ()
transcodeFiles (path:paths) = do
  transcode path
  transcodeFiles paths