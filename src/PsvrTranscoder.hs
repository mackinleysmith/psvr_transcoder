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

transcode :: String -> IO Bool
transcode path_str =
  case toText maybe_working_base of
    Right working_base ->
      runFFmpegOn VideoFile
        { pathStr = path_str
        , workingBase = working_base
        , maybeWorkingExt = maybe_working_ext
        , inputFile = input_file }
    _ -> return False
  where
    input_file = decodeString path_str
    (maybe_working_base, maybe_working_ext) = splitExtension input_file

runFFmpegOn :: VideoFile -> IO Bool
runFFmpegOn video_file = (== ExitSuccess) <$> system ( command video_file )

command :: VideoFile -> String
command video_file @ (VideoFile input_path _ _ _) =
  "ffmpeg -i \"" ++ input_path ++ "\" " ++ options ++ " \"" ++ output_file ++ "\""
  where
    options = "-vcodec libx264 -profile:v high -level:v 4.2 -s 1920x1080 -preset slow -crf 18 -c:a libfdk_aac -vbr 5"
    output_file = outputFileFor video_file

outputFileFor :: VideoFile -> String
outputFileFor (VideoFile _ working_base maybe_working_ext _) =
  T.unpack working_base ++ "_psvr_180_sbs." ++ original_extension
  where
    original_extension = case maybe_working_ext of Just ext -> T.unpack ext; _ -> ""
