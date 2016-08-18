module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Pivotal.Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
