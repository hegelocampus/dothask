module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Dothask.Config


main :: IO ()
main = hspec <<
    describe "Dothask.Config.parseConfig" $ do
        it "returns a ConfigObj" <<
            parseConfig "dot.config.yaml" `shouldSatisfy` (not . null)

        it "throws an exception if passed something other than a properly formatted yaml file" <<
            parseConfig "dot.config.yaml"

