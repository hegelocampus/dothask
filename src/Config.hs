{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Config
  (
    parseConfig
  ) where

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Data.HashMap.Strict as HM

data Config =
  Config {
    defaults  :: Y.Object
  , link      :: HM.HashMap FilePath (Maybe FilePath)
  } deriving stock (Eq, Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    v .: "defaults" <*>
    v .: "link"
  parseJSON _ = fail "Expected Object for Config value"

-- It may make sense for the file path to be passed in as an argument so that the user
-- can pass in a custom config path
parseConfig :: String -> IO Config
parseConfig p = do
  -- res is the type of (MonadIO FromJSON)
  res <- Y.decodeFileThrow p
  -- This doesn't work becauce both the functions passed into either need to
  -- return the same type
  return (res :: Config)


