{-# LANGUAGE OverloadedStrings #-}

module PsvrTranscoder (transcode) where

import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS
import           System.Exit
import           System.Process

transcode :: String -> IO Bool
transcode path_str =
  case toText maybe_working_base of
    Right working_base -> runFFmpegOn working_base input_file maybe_working_ext
    _ -> return False
  where
    input_file = decodeString path_str
    (maybe_working_base, maybe_working_ext) = splitExtension input_file

runFFmpegOn :: T.Text -> Filesystem.Path.CurrentOS.FilePath -> Maybe T.Text -> IO Bool
runFFmpegOn working_base input_file maybe_working_ext =
  (== ExitSuccess) <$> system ( command working_base maybe_working_ext input_file )

command :: T.Text -> Maybe T.Text -> Filesystem.Path.CurrentOS.FilePath -> String
command working_base maybe_working_ext input_file =
  "ffmpeg -i \"" ++ encodeString input_file ++ "\" " ++ options ++ " \"" ++ output_file ++ "\""
  where
    options = "-vcodec libx264 -profile:v high -level:v 4.2 -s 1920x1080 -preset slow -crf 18 -c:a libfdk_aac -vbr 5"
    output_file = outputFileFor (decodeString $ T.unpack working_base) maybe_working_ext

outputFileFor :: Filesystem.Path.CurrentOS.FilePath -> Maybe T.Text -> String
outputFileFor working_base maybe_working_ext =
  encodeString working_base ++ "_psvr_180_sbs." ++ original_extension
  where
    original_extension = case maybe_working_ext of Just ext -> T.unpack ext; _ -> ""