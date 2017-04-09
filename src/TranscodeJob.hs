module TranscodeJob where

import           Config

data TranscodeJob = TranscodeJob
  { shouldEquirectangularize :: Bool }

transcodeJobFromConfig :: Config -> Maybe TranscodeJob
transcodeJobFromConfig config = Just TranscodeJob
  { shouldEquirectangularize = equirectangularize config }
