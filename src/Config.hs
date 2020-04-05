{-# LANGUAGE OverloadedStrings #-}

module Config
  (
    parseConfig
  ) where

import qualified Data.Yaml as Y

main :: IO ()
main = do
  res <- Y.decodeFileThrow "../dot.config.yaml" -- This path should not be hardcoded in a later verison
  print res
