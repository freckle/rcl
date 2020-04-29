module Rcl.SillyArrayMapSpec
  ( spec
  )
where

import RIO

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Version
import Rcl.PackageName
import Rcl.Resolver
import qualified Rcl.SillyArrayMap as SillyArrayMap
import Test.Hspec

spec :: Spec
spec = do
  describe "FromJSON" $ do
    it "parses this silly diff syntax" $ do
      let
        sillyJson = mconcat
          [ "["
          , "  [\"foo\", {\"lts-x\": \"1.2.0\", \"lts-y\": \"1.3.0\"}],"
          , "  [\"bar\", {\"lts-x\": \"2.2.0\", \"lts-y\": \"2.2.1\"}]"
          , "]"
          ]

        decoded :: Either String [(PackageName, HashMap Resolver Version)]
        decoded = SillyArrayMap.toList <$> eitherDecode sillyJson

      decoded `shouldBe` Right
        [ ( "foo"
          , HashMap.fromList
            [("lts-x", makeVersion [1, 2, 0]), ("lts-y", makeVersion [1, 3, 0])]
          )
        , ( "bar"
          , HashMap.fromList
            [("lts-x", makeVersion [2, 2, 0]), ("lts-y", makeVersion [2, 2, 1])]
          )
        ]
