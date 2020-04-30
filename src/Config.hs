{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Config
    ( parseConfig
    , removeMaybes
    , weightedUnion
    , DefaultsConfig (..)
    , LinkConfig (..)
    , ConfigObj (..)
    , StrictLink (..)
    ) where

import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import GHC.Generics

data DefaultsConfig = DefaultsConfig
    { linkConfig   :: !LinkConfig
    , createConfig :: !(Maybe Y.Object)
    } deriving stock (Generic, Show)

instance Y.FromJSON DefaultsConfig

data LinkConfig = LinkConfig
    { createCfg   :: !(Maybe Bool)    -- ^ Create parent dirs (default: false)
    , pathCfg     :: !(Maybe String)  -- ^ The source of the link (default: false)
    , relinkCfg   :: !(Maybe Bool)    -- ^ Remove target symlink (default: false)
    , forceCfg    :: !(Maybe Bool)    -- ^ Force removal of target (default: false)
    , relativeCfg :: !(Maybe Bool)    -- ^ Relative path to source (default: false)
    } deriving stock (Generic, Show)

instance Y.FromJSON LinkConfig

data StrictLink = StrictLink
    { create   :: !Bool    -- ^ Create parent dirs (default: false)
    , path     :: !String  -- ^ The source of the link (default: false)
    , relink   :: !Bool    -- ^ Remove target symlink (default: false)
    , force    :: !Bool    -- ^ Force removal of target (default: false)
    , relative :: !Bool    -- ^ Relative path to source (default: false)
    } deriving stock (Generic, Show)

-- | TODO Set defaults to be Maybe DefaultsConfig.
data ConfigObj = ConfigObj
    { defaults :: !DefaultsConfig                                 -- ^ Defaults config object
    -- Order of operations
    , link     :: !(HM.HashMap FilePath (Maybe (Either String LinkConfig)))  -- ^ Link configuration
    } deriving stock (Generic, Show)

instance Y.FromJSON ConfigObj

-- | Combine two Links where values in the first that are Nothing are replaced
-- with the value in the second.
-- Uses Data.HashMap.Strict.unionWith.
weightedUnion :: LinkConfig -> LinkConfig -> LinkConfig
weightedUnion x y = LinkConfig
    { createCfg   = leftIfMaybe (createCfg y) $ createCfg x
    , pathCfg     = leftIfMaybe (pathCfg y) $ pathCfg x
    , relinkCfg   = leftIfMaybe (relinkCfg y) $ relinkCfg x
    , forceCfg    = leftIfMaybe (forceCfg y) $ forceCfg x
    , relativeCfg = leftIfMaybe (relativeCfg y) $ relativeCfg x
    }
  where leftIfMaybe v1 v2 = if isJust v1 then v1 else v2

removeMaybes :: LinkConfig -> StrictLink
removeMaybes x = StrictLink
    { create = fromMaybe False $ createCfg x
    , path = fromMaybe "" $ pathCfg x
    , relink = fromMaybe False $ relinkCfg x
    , force = fromMaybe False $ forceCfg x
    , relative = fromMaybe False $ relativeCfg x
    }

-- TODO: Parse DefaultsConfig object to fill missing values with defaults
-- | Parse config file into Config object
parseConfig :: String -> IO ConfigObj
parseConfig p = Y.decodeFileThrow p >>= \res -> return (res :: ConfigObj)

