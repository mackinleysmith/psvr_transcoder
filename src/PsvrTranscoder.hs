{-# LANGUAGE OverloadedStrings #-}

module PsvrTranscoder (transcode) where

import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS
import           System.Exit
import           System.Process

data VideoFile = VideoFile
  { pathStr         :: String
  , workingBase     :: T.Text
  , maybeWorkingExt :: Maybe T.Text
  , inputFile       :: Filesystem.Path.CurrentOS.FilePath }

transcode :: String -> IO ()
transcode path_str =
  case toText maybe_working_base of
    Right working_base -> do
      _ <- runFFmpegOn
        VideoFile
          { pathStr = path_str
          , workingBase = working_base
          , maybeWorkingExt = maybe_working_ext
          , inputFile = input_file }
      return ()
    _ -> return ()
  where
    input_file = decodeString path_str
    (maybe_working_base, maybe_working_ext) = splitExtension input_file

runFFmpegOn :: VideoFile -> IO Bool
runFFmpegOn video_file = (== ExitSuccess) <$> system ( command video_file )

command :: VideoFile -> String
command video_file @ VideoFile { pathStr = input_path } =
  "ffmpeg -i \"" ++ input_path ++ "\" " ++ options ++ " \"" ++ output_file ++ "\""
  where
    options = "-vcodec libx264 -profile:v high -level:v 4.2 -s 1920x1080 -preset slow -crf 18 -c:a libfdk_aac -vbr 5"
    output_file = outputFileFor video_file

outputFileFor :: VideoFile -> String
outputFileFor VideoFile { workingBase = working_base, maybeWorkingExt = maybe_working_ext } =
  filename ++ "_psvr_180_sbs." ++ original_extension
  where
    filename = T.unpack working_base
    original_extension = case maybe_working_ext of Just ext -> T.unpack ext; _ -> ""
