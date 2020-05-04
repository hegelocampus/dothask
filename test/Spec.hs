module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.HashMap.Strict as HM
import Dothask.Config hiding (empty)
import qualified Dothask.Config as C (empty)


main :: IO ()
main = hspec <$>
    describe "Dothask.Config.parseConfig" $ do
        it "does not return an empty ConfigObj" $
            parseConfig "dot.config.yaml" `shouldNotReturn` C.empty

        context "When passed something other than a yaml file" .
            it "throws an exception" $
                parseConfig "not.a.yaml" `shouldThrow` anyException

