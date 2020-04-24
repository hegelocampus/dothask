{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Config
    (
      parseConfig
    , LinkConfig (..)
    , ConfigObj (..)
    ) where

import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as HM
import GHC.Generics

newtype DefaultsConfig = DefaultsConfig
    { linkConfig :: !LinkConfig } deriving stock (Generic, Show)

data LinkConfig = LinkConfig
    { create   :: !(Maybe Bool)    -- ^ Create parent dirs (default: false)
    , path     :: !(Maybe String)  -- ^ The source of the link (default: false)
    , relink   :: !(Maybe Bool)    -- ^ Remove target symlink (default: false)
    , force    :: !(Maybe Bool)    -- ^ Force removal of target (default: false)
    , relative :: !(Maybe Bool)    -- ^ Relative path to source (default: false)
    } deriving stock (Generic, Show)

instance Y.FromJSON LinkConfig

data ConfigObj = ConfigObj
    { defaults :: !DefaultsConfig                                 -- ^ Defaults config object
    -- Order of operations
    , link     :: !(HM.HashMap
                      FilePath Maybe (Either String LinkConfig))  -- ^ Link configuration
    } deriving stock (Generic, Show)

instance Y.FromJSON ConfigObj

-- TODO: Parse DefaultsConfig object to fill missing values with defaults
-- | Parse config file into Config object
parseConfig :: String -> IO ConfigObj
parseConfig p = Y.decodeFileThrow p >>= \res -> return (res :: ConfigObj)

