{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Config
  (
    parseConfig
  ) where

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Data.HashMap.Strict as HM
--import Data.Maybe (fromMaybe)

data Config =
  Config {
    defaults  :: Y.Object
  , link      :: HM.HashMap FilePath FilePath
  } deriving stock (Eq, Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    v .: "defaults" <*>
    v .: "link"
  parseJSON _ = fail "Expected Object for Config value"

-- It may make sense for the file path to be passed in as an argument so that the user
-- can pass in a custom config path
parseConfig :: IO ()
parseConfig = do
  -- The format of the imoported file looks like:
  -- [
  --    fromList [("defaults", Object (fromList ["link", Object (fromList
  --      [("force", Bool True), ("create", Bool True)]
  --    )])],
  --    fromList ["link", Object ("~/.foo",Null),.("~/.bar,Null),..]
  -- ]
  res <- Y.decodeFileThrow "dot.config.yaml"
  --let lnks = fromMaybe HM.empty $ (res .: "link")
  print (res :: Config)
