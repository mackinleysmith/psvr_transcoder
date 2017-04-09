module VideoFile where

import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS
import           TranscodeJob

data VideoFile = VideoFile
  { pathStr      :: String
  , filePath     :: Filesystem.Path.CurrentOS.FilePath
  , baseName     :: T.Text
  , fileExt      :: Maybe T.Text
  , transcodeJob :: TranscodeJob }

parseVideoFile :: String -> TranscodeJob -> Maybe VideoFile
parseVideoFile path_str transcode_job =
  case toText maybe_base_name of
    Right base_name -> Just VideoFile
        { pathStr = path_str
        , filePath = input_file
        , baseName = base_name
        , fileExt = maybe_ext
        , transcodeJob = transcode_job }
    _ -> Nothing
  where
    input_file = decodeString path_str
    (maybe_base_name, maybe_ext) = splitExtension input_file
