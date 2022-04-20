module Rcl.VersionDiffSpec
  ( spec
  ) where

import RIO

import Data.Version
import Rcl.VersionDiff
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "versionDiff" $ do
    it "pickle and unpickle" $ property $ \(v1, v2) ->
      let VersionDiff {..} = versionDiff v1 v2
      in
        (showVersion v1 == (commonPrefix <> fromSuffix))
          && (showVersion v2 == (commonPrefix <> toSuffix))

    it "diffs a minor change"
      $ let diff = versionDiff (makeVersion [1, 2, 3]) (makeVersion [1, 2, 4])
        in
          diff `shouldBe` VersionDiff
            { commonPrefix = "1.2."
            , fromSuffix = "3"
            , toSuffix = "4"
            }

    it "diffs a major change"
      $ let diff = versionDiff (makeVersion [1, 1, 0]) (makeVersion [1, 2, 0])
        in
          diff `shouldBe` VersionDiff
            { commonPrefix = "1."
            , fromSuffix = "1.0"
            , toSuffix = "2.0"
            }

    it "diffs an epic change"
      $ let diff = versionDiff (makeVersion [4, 1, 0]) (makeVersion [5, 1, 0])
        in
          diff `shouldBe` VersionDiff
            { commonPrefix = ""
            , fromSuffix = "4.1.0"
            , toSuffix = "5.1.0"
            }

    it "diffs a place change"
      $ let
          diff = versionDiff (makeVersion [2, 12, 0]) (makeVersion [2, 14, 0])
        in
          diff `shouldBe` VersionDiff
            { commonPrefix = "2.1"
            , fromSuffix = "2.0"
            , toSuffix = "4.0"
            }
