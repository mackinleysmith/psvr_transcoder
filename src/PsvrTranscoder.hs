{-# LANGUAGE OverloadedStrings #-}

module PsvrTranscoder (transcode) where

import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS
import           System.Exit
import           System.Process

transcode :: String -> IO Bool
transcode path_str = do
  let maybe_input_file = decodeString path_str
  case toText maybe_input_file of
    Left _ -> return False
    Right input_file -> do
      let (maybe_working_base, maybe_working_ext) = splitExtension maybe_input_file

      case toText maybe_working_base of
        Right working_base -> thing working_base maybe_input_file maybe_working_ext
        _ -> return False

thing :: T.Text -> Filesystem.Path.CurrentOS.FilePath -> Maybe T.Text -> IO Bool
thing working_base input_file maybe_working_ext = do
  exit_code <- system $ command working_base maybe_working_ext input_file
  return $ exit_code == ExitSuccess

command :: T.Text -> Maybe T.Text -> Filesystem.Path.CurrentOS.FilePath -> String
command working_base maybe_working_ext input_file =
  "ffmpeg -i \"" ++ encodeString input_file ++ "\" -vcodec libx264 -profile:v high -level:v 4.2 -s 1920x1080 -preset slow -crf 18 -c:a libfdk_aac -vbr 5 \"" ++ output_file ++ "\""
  where
    output_file = T.unpack $ working_base `T.append` T.pack "_psvr_180_sbs" `T.append` T.pack "." `T.append` ( case maybe_working_ext of Just ext -> ext; _ -> T.pack "" )
