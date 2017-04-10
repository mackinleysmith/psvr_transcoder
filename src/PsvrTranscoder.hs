{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module PsvrTranscoder (transcode) where

import           Data.Array
import qualified Data.ByteString           as B
import           Data.FileEmbed            (embedFile)
import qualified Data.Text                 as T
import           System.Directory          (createDirectoryIfMissing,
                                            getDirectoryContents,
                                            removeDirectoryRecursive,
                                            removeFile)
import           System.Process
import           Text.Regex
import           Text.Regex.Base
import           TranscodeJob
import           VideoFile

transcode :: String -> TranscodeJob -> IO ()
transcode path_str job =
  case parseVideoFile path_str job of
    Just video_file -> applyTranscodeJobTo video_file
    _ -> return ()

applyTranscodeJobTo :: VideoFile -> IO ()
applyTranscodeJobTo video_file @ VideoFile { pathStr = path_str, transcodeJob = TranscodeJob { shouldEquirectangularize = True } } = do
  (_, _, info) <- readProcessWithExitCode "ffmpeg" ["-i", path_str] ""
  case findResolution $ lines info of
    Just [width, height] -> do
      let half_width = width `div` 2
      generateProjectionMapsFor [half_width, height]

      _ <- createDirectoryIfMissing False "./x_frames"
      _ <- createDirectoryIfMissing False "./y_frames"

      _ <- system $ "ffmpeg -i \"" ++ path_str ++ "\" -filter:v \"crop=" ++ show half_width ++ ":" ++ show height ++ ":0:0\"  x_frames/frame%04d.jpg -filter:v \"crop=" ++ show half_width ++ ":" ++ show height ++ ":" ++ show half_width ++ ":0\" y_frames/frame%04d.jpg -hide_banner"

      _ <- system "ffmpeg -i ./x_frames/frame%04d.jpg -i ./xmap.pgm -i ./ymap.pgm -filter_complex remap -y ./x_frames/frame%04d_corrected.jpg"
      _ <- system "ffmpeg -i ./y_frames/frame%04d.jpg -i ./xmap.pgm -i ./ymap.pgm -filter_complex remap -y ./y_frames/frame%04d_corrected.jpg"

      _ <- system $ "ffmpeg -r 59.94 -f image2 -i ./x_frames/frame%04d_corrected.jpg -r 59.94 -f image2 -i ./y_frames/frame%04d_corrected.jpg -filter_complex \"[0:v]setpts=PTS-STARTPTS, pad=iw*2:ih[bg]; [1:v]setpts=PTS-STARTPTS[fg]; [bg][fg]overlay=w\" -vcodec libx264 -profile:v high -level:v 4.2 -s 1920x1080 -preset slow -crf 18 -vbr 5 -f mpegts - | ffmpeg -i - -i \"" ++ path_str ++ "\" -map 0 -map 1 -map -1:v:0 -c copy \"" ++ outputFileFor video_file ++ "\""

      removeFile "./xmap.pgm"
      removeFile "./ymap.pgm"
      removeDirectoryRecursive "./x_frames"
      removeDirectoryRecursive "./y_frames"

      return ()
    _ -> putStrLn "Failed to parse resolution!"

applyTranscodeJobTo video_file = runFFmpegOn video_file $ commandFor video_file

runFFmpegOn :: VideoFile -> String -> IO ()
runFFmpegOn _ command = do
  _ <- system command
  return ()

commandFor :: VideoFile -> String
commandFor VideoFile
  { pathStr = path_str
  , transcodeJob = TranscodeJob { shouldEquirectangularize = True } } =
    "ffmpeg -i \"" ++ path_str ++ "\""
commandFor video_file = commandToTranscodeDirectlyFor video_file

commandToTranscodeDirectlyFor :: VideoFile -> String
commandToTranscodeDirectlyFor video_file @ VideoFile { pathStr = path_str } =
  "ffmpeg -i \"" ++ path_str ++ "\" " ++ options ++ " \"" ++ output_file ++ "\""
  where
    options = "-vcodec libx264 -profile:v high -level:v 4.2 -s 1920x1080 -preset slow -crf 18 -c:a libfdk_aac -vbr 5"
    output_file = outputFileFor video_file

outputFileFor :: VideoFile -> String
outputFileFor = addSuffixToFilename "psvr_180_sbs"

addSuffixToFilename :: String -> VideoFile -> String
addSuffixToFilename suffix VideoFile { baseName = working_base, fileExt = maybe_ext } =
  filename ++ "_" ++ suffix ++ original_extension
  where
    filename = T.unpack working_base
    original_extension =
      case maybe_ext of
        Just ext -> '.' : T.unpack ext
        _ -> ""

generateProjectionMapsBinary :: B.ByteString
generateProjectionMapsBinary = $(embedFile "bin/generate_projection_maps")

generateProjectionMapsFor :: [Int] -> IO ()
generateProjectionMapsFor [width, height] =
  generateProjectionMaps $ "--xmap " ++ xmap_path ++ " --ymap " ++ ymap_path ++ " --rows " ++ show height ++ " --cols " ++ show width ++ " --width " ++ show width ++ " --height " ++ show height
  where
    xmap_path = "./xmap.pgm"
    ymap_path = "./ymap.pgm"
generateProjectionMapsFor _ = return ()

generateProjectionMaps :: String -> IO ()
generateProjectionMaps args = do
  B.writeFile "./generate_projection_maps" generateProjectionMapsBinary
  _ <- system "chmod +x ./generate_projection_maps"
  _ <- system $ "./generate_projection_maps " ++ args
  removeFile "./generate_projection_maps"
  return ()

findResolution :: [String] -> Maybe [Int]
findResolution [] = Nothing
findResolution (l:ls) = do
  let finder = mkRegex "Stream.*: Video: .*, ([0-9]+)x([0-9]+), "
      matches = matchAllText finder l

  if not $ Prelude.null matches then
    Just $ map (read . fst) . drop 1 . elems $ head matches
  else findResolution ls