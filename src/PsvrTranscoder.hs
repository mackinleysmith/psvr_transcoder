{-# LANGUAGE OverloadedStrings #-}

module PsvrTranscoder (transcode) where

import Filesystem.Path.CurrentOS
import System.Exit
import System.Process
import qualified Data.Text as T

transcode :: String -> IO Bool
transcode path_str = do
  let maybe_input_file = decodeString path_str
  case toText maybe_input_file of
    Left _ -> return False
    Right input_file -> do
      let (maybe_working_base, maybe_working_ext) = splitExtension maybe_input_file

      case toText maybe_working_base of
        Left _ -> return False
        Right working_base -> do
          let output_file = working_base `T.append` (T.pack "_psvr_180_sbs") `T.append` T.pack "." `T.append` ( case maybe_working_ext of Just ext -> ext; _ -> T.pack "" )
          let command = (T.pack "ffmpeg -i \"") `T.append` input_file `T.append` (T.pack "\" -vcodec libx264 -profile:v high -level:v 4.2 -s 1920x1080 -preset slow -crf 18 -c:a libfdk_aac -vbr 5 \"") `T.append` output_file `T.append` (T.pack "\"")
          exit_code <- system $ T.unpack command
          return $ exit_code == ExitSuccess