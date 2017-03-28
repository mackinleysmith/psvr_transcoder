{-# LANGUAGE OverloadedStrings #-}

module PsvrTranscoder (transcode) where

import qualified Data.Text      as T
import           System.Process
import           VideoFile

transcode :: String -> IO ()
transcode path_str =
  case parseVideoFile path_str of
    Just videoFile -> runFFmpegOn videoFile
    _ -> return ()

runFFmpegOn :: VideoFile -> IO ()
runFFmpegOn video_file = do
  system $ commandFor video_file
  return ()

commandFor :: VideoFile -> String
commandFor video_file @ VideoFile { pathStr = path_str } =
  "ffmpeg -i \"" ++ path_str ++ "\" " ++ options ++ " \"" ++ output_file ++ "\""
  where
    options = "-vcodec libx264 -profile:v high -level:v 4.2 -s 1920x1080 -preset slow -crf 18 -c:a libfdk_aac -vbr 5"
    output_file = outputFileFor video_file

outputFileFor :: VideoFile -> String
outputFileFor VideoFile { baseName = working_base, fileExt = maybe_ext } =
  filename ++ "_psvr_180_sbs" ++ original_extension
  where
    filename = T.unpack working_base
    original_extension =
      case maybe_ext of
        Just ext -> '.' : T.unpack ext
        _ -> ""
