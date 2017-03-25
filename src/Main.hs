module Main where

import           Options.Applicative
import           PsvrTranscoder      (transcode)

data Config = Config
  { quiet   :: Bool
  , monitor :: Bool
  , files   :: [String] }

config :: Parser Config
config = Config
    <$> switch
        ( long "quiet"
       <> short 'q'
       <> help "Whether to be quiet" )
    <*> switch
        ( long "monitor"
       <> short 'm'
       <> help "Monitor for changes to a directory" )
    <*> some ( argument str $ metavar "FILES OR DIRECTORIES..." )

main :: IO ()
main = transcode_each =<< execParser opts
  where
    opts = info ( config <**> helper)
      ( fullDesc
     <> progDesc "Encode video files for usage with the PlayStation VR through LittlStar."
     <> header "psvr_transcode - a simple FFmpeg-based utility to encode video for PSVR" )

transcode_each :: Config -> IO ()
transcode_each (Config _ _ []) = return ()
transcode_each (Config quiet' monitor' (path:paths)) = do
  transcode path
  transcode_each $ Config quiet' monitor' paths
