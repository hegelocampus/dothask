{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Turtle (FilePath, Text, decodeString)
import Prelude hiding (FilePath)

import Dothask (parsePath)
import Dothask.Config hiding (empty)
import qualified Dothask.Config as C (empty)


main :: IO ()
main = hspec $ do
    describe "Dothask.Config.parseConfig" $ do
        it "does not return an empty ConfigObj" $
            parseConfig "dot.config.yaml" `shouldNotReturn` C.empty

        context "When passed something other than a yaml file" .
            it "throws an exception" $
                parseConfig "not.a.yaml" `shouldThrow` anyException

    describe "Dothask.parsePath" $ do
        let userHome = "/home/example" :: FilePath
        let cPrse = parsePath userHome
        context "When the passed in filepath that starts with a tilde" $ do
            it "replaces the tilde with the passed in string in a simple filepath" $ do
                let pth = "~/.profile"
                cPrse pth `shouldBe` ("/home/example/.profile" :: FilePath)

            it "replaces the tilde with the passed in string in a complex filepath" $ do
                let pth = "~/.config/example_program/example.config"
                cPrse pth `shouldBe` ("/home/example/.config/example_program/example.config" :: FilePath)

        context "When the passed in filepath that does not start with a tilde" $ do
            let fPath = "/home/user/filepath/.config"
            it "returns the original filepath" $ do
                cPrse fPath `shouldBe` ("/home/user/filepath/.config" :: FilePath)
                cPrse fPath `shouldNotBe` ("/home/example/home/user/filepath/.config" :: FilePath)

    describe "Dothask.cleanTargetFile" $ do
        context "When noConfirm is True" $ do
            it "does not ask before removing an existing regular file" $ do
                pending
        context "When noConfirm is False" $ do
            it "prompts the user before removing an existing regular file" $ do
                pending

    describe "Dothask.cleanTargetLink" $ do
        it "removes an existing symlink and returns true" $ do
            pending
        it "just returns True for an empty path" $ do
            pending
        it "does not remove an existing regular file and returns false" $
            pending

    describe "Dothask.checkTree" $ do
        context "When allowed to create a directory" $ do
            it "creates a directory if needed" $ do
                pending
            it "returns True" $ do
                pending
        context "When not allowed to create a directory" $ do
            it "returns True if the directory does not exist" $ do
                pending
            it "returns False if the directory does not exist" $ do
                pending
