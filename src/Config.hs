{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Config
    (
      parseConfig
    , LinkConfig (..)
    , ConfigObj (..)
    ) where

-- TODO: Move most of this function into the dothask.hs file, all that should
-- be happening here is the initiall Parsing of the config and setting of
-- default values
-- Can then import the LinkConfig and Config types into the main file for meat
-- and potatoes actions

import qualified Data.Yaml as Y
--import Data.Yaml (FromJSON(..))
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

-- TODOMAYBE: Move type definitions to their own files to facilitate use
data ConfigObj = ConfigObj
    { defaults :: !DefaultsConfig                                 -- ^ Defaults config object
    , link     :: !(HM.HashMap
                      FilePath Maybe (Either String LinkConfig))  -- ^ Link configuration
    } deriving stock (Generic, Show)

instance Y.FromJSON ConfigObj

-- | Parse config file into Config object
parseConfig :: String -> IO ConfigObj
parseConfig p = Y.decodeFileThrow p >>= \res -> return (res :: ConfigObj)

