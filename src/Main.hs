module Main where

import PsvrTranscoder (transcode)
import Options.Applicative

data Config = Config
  { quiet  :: Bool
  , files  :: [String] }

config :: Parser Config
config = Config
    <$> switch
        ( long "quiet"
       <> short 'q'
       <> help "Whether to be quiet" )
    <*> some ( argument str $ metavar "FILES..." )

main :: IO ()
main = transcode_each =<< execParser opts
  where
    opts = info ( config <**> helper)
      ( fullDesc
     <> progDesc "Encode video files for usage with the PlayStation VR through LittlStar."
     <> header "psvr_transcode - a simple FFmpeg-based utility to encode video for PSVR" )

transcode_each :: Config -> IO ()
transcode_each (Config _ []) = return ()
transcode_each (Config quiet' (path:paths)) = do
  transcode path
  transcode_each $ Config quiet' paths