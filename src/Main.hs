module Main where

import           Config
import           PsvrTranscoder      (transcode)

main :: IO ()
main = transcodeEach =<< parseConfig

transcodeEach :: Config -> IO ()
transcodeEach Config { files = [] } = return ()
transcodeEach (Config quiet' monitor' (path:paths)) = do
  transcode path
  transcodeEach ( Config quiet' monitor' paths )
