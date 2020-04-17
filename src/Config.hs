{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Config (parseConfig) where

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..))
import Data.HashMap.Strict as HM
import GHC.Generics

data LinkConfig =
  LinkConfig {
    create   :: Bool -- Create parent directories as needed (default: null)
  , path     :: FilePath -- The source of the symlink (default: false)
  , relink   :: Bool -- Remove the old target if it is a symlink (default: false)
  , force    :: Bool -- Force removal of old target and create new link (default: false)
  , relative :: Bool -- Use relative path to the source (default: false, absolute links)
  } deriving (Generic, Show)

instance FromJSON LinkConfig

data Config =
  Config {
    defaults  :: Y.Object
  , link      :: HM.HashMap FilePath (Maybe LinkConfig)
  } deriving (Generic, Show)

instance FromJSON Config

-- TODO: Create setDefaults function
-- setDefaults should use Data.Yaml (.!=) to set missing default values

-- It may make sense for the file path to be passed in as an argument so that the user
-- can pass in a custom config path
parseConfig :: String -> IO Config
parseConfig p = do
  -- res is the type of (MonadIO FromJSON)
  res <- Y.decodeFileThrow p
  return (res :: Config)

