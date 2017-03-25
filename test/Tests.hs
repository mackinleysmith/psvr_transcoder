{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import PsvrTranscoder (transcode)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "PsvrTranscoder" $
  describe "transcode" $ for_ cases test
  where
    test Case{..} = it description assertion
      where
        assertion  = transcode testBoard `shouldBe` expected
        testBoard  = filter (/=' ') <$> board

data Case = Case { description :: String
                 , board       :: [String]
                 , expected    :: Maybe Mark
                 }