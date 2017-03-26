{-# LANGUAGE OverloadedStrings #-}

module PsvrTranscoder (transcode) where

import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS
import           System.Exit
import           System.Process

data VideoFile = VideoFile
  { pathStr  :: String
  , filePath :: Filesystem.Path.CurrentOS.FilePath
  , baseName :: T.Text
  , fileExt  :: Maybe T.Text }

transcode :: String -> IO ()
transcode path_str =
  case toText maybe_working_base of
    Right base_name -> do
      _ <- runFFmpegOn
        VideoFile
          { pathStr = path_str
          , filePath = input_file
          , baseName = base_name
          , fileExt = maybe_ext }
      return ()
    _ -> return ()
  where
    input_file = decodeString path_str
    (maybe_working_base, maybe_ext) = splitExtension input_file

runFFmpegOn :: VideoFile -> IO Bool
runFFmpegOn video_file = (== ExitSuccess) <$> system ( commandFor video_file )

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
