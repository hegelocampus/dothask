module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Dothask.Config


main :: IO ()
main = hspec $ do
    describe "Dothask.Config.parseConfig" $ do
        it "returns a ConfigObj" $ do
            parseConfig "dot.config.yaml" `shouldSatisfy` (not . null)

        it "throws an exception if passed something other than a properly formatted yaml file" $ do
            parseConfig "not.a.yaml" `shouldThrow` anyException

