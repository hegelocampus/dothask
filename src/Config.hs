{-# LANGUAGE OverloadedStrings #-}

module Config
  (
    parseConfig
  ) where

import qualified Data.Yaml as Y

parseConfig :: IO ()
parseConfig = do
  -- The format of the imoported file looks like:
  -- [
  --    fromList [("defaults", Object (fromList ["link",Object (fromList
  --      [("force", Bool True), ("create", Bool True)]
  --    )])],
  --    fromList [("~/.foo",Null),.("~/.bar,Null),..]
  -- ]
  res <- Y.decodeFileThrow "dot.config.yaml" -- This path should not be hardcoded in a later verison
  print (res :: [Y.Object])
