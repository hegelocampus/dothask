{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Dothask.Config
    (
    -- * Types
    ConfigObj (..)
    , DefaultsConfig (..)
    , LinkConfig (..)
    , MaybeLinkCfg
    , StrictLink (..)
      -- * Utility functions for dealing with config records
    , parseConfig
    , buildLinkCfg
    , removeMaybes
    , weightedUnion
    , empty
    ) where

import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import GHC.Generics
import Turtle (Text, FilePath, decodeString)
import Prelude hiding (FilePath)

-- | TODO Set defaults to be Maybe DefaultsConfig.
data ConfigObj = ConfigObj
    { defaults :: !DefaultsConfig                  -- ^ Defaults config object
    , link     :: !(HM.HashMap Text MaybeLinkCfg)  -- ^ Link configuration
    } deriving stock (Generic, Show, Eq)

instance Y.FromJSON ConfigObj

type MaybeLinkCfg = Maybe LinkConfig

data DefaultsConfig = DefaultsConfig
    { linkConfig   :: !LinkConfig
    , createConfig :: !(Maybe Y.Object)
    } deriving stock (Generic, Show, Eq)

instance Y.FromJSON DefaultsConfig

data LinkConfig = LinkConfig
    { createCfg   :: !(Maybe Bool)    -- ^ Create parent dirs (default: false)
    , pathCfg     :: !(Maybe String)  -- ^ The source of the link (default: false)
    , relinkCfg   :: !(Maybe Bool)    -- ^ Remove target symlink (default: false)
    , forceCfg    :: !(Maybe Bool)    -- ^ Force removal of target (default: false)
    , relativeCfg :: !(Maybe Bool)    -- ^ Relative path to source (default: false)
    } deriving stock (Generic, Show, Eq)

instance Y.FromJSON LinkConfig

-- | Strict Link with no Maybe values
data StrictLink = StrictLink
    { create   :: !Bool    -- ^ Create parent dirs (default: false)
    , path     :: !FilePath  -- ^ The source of the link (default: false)
    , relink   :: !Bool    -- ^ Remove target symlink (default: false)
    , force    :: !Bool    -- ^ Force removal of target (default: false)
    , relative :: !Bool    -- ^ Relative path to source (default: false)
    } deriving stock (Generic, Show)

-- | This should never be used outside of testing
empty :: ConfigObj
empty = ConfigObj
    { defaults = DefaultsConfig
        { linkConfig = LinkConfig
            { createCfg   = Just False
            , pathCfg     = Just ""
            , relinkCfg   = Just False
            , forceCfg    = Just False
            , relativeCfg = Just False
            }
            , createConfig = Nothing
        }
    , link = HM.empty :: HM.HashMap Text MaybeLinkCfg
    }

-- | Build a link from a string representing the path and a LinkConfig object.
-- TODOMAYBE: Empty string to create path to "./filename", this could be
-- implemented in a different way
buildLinkCfg :: String -> LinkConfig -> LinkConfig
buildLinkCfg str cfg = LinkConfig
    { createCfg   = createCfg cfg
    , pathCfg     = Just str
    , relinkCfg   = relinkCfg cfg
    , forceCfg    = forceCfg cfg
    , relativeCfg = relativeCfg cfg
    }

-- | Combine two Links where values in the first that are Nothing are replaced
-- with the value in the second.
-- Uses Data.HashMap.Strict.unionWith.
weightedUnion :: LinkConfig -> LinkConfig -> LinkConfig
weightedUnion x y = LinkConfig
    { createCfg   = leftIfMaybe (createCfg y) (createCfg x)
    , pathCfg     = leftIfMaybe (pathCfg y) (pathCfg x)
    , relinkCfg   = leftIfMaybe (relinkCfg y) (relinkCfg x)
    , forceCfg    = leftIfMaybe (forceCfg y) (forceCfg x)
    , relativeCfg = leftIfMaybe (relativeCfg y) (relativeCfg x)
    }
  where leftIfMaybe v1 v2 = if isJust v1 then v1 else v2

removeMaybes :: LinkConfig -> StrictLink
removeMaybes x = StrictLink
    { create = fromMaybe False $ createCfg x
    , path = decodeString . fromMaybe "" $ pathCfg x
    , relink = fromMaybe False $ relinkCfg x
    , force = fromMaybe False $ forceCfg x
    , relative = fromMaybe False $ relativeCfg x
    }

-- TODO: Parse DefaultsConfig object to fill missing values with defaults
-- | Parse config file into Config object
parseConfig :: String -> IO ConfigObj
parseConfig pth = Y.decodeFileThrow pth >>= \res -> return (res :: ConfigObj)

