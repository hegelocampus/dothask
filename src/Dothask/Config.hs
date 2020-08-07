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

data ConfigObj = ConfigObj
    { defaults :: !DefaultsConfig                  -- ^ Defaults config object
    , link     :: !(HM.HashMap Text MaybeLinkCfg)  -- ^ Link configuration
    , dirs     :: !(Maybe [Text])                  -- ^ Link configuration
    } deriving stock (Generic, Show, Eq)

instance Y.FromJSON ConfigObj

type MaybeLinkCfg = Maybe LinkConfig

data DefaultsConfig = DefaultsConfig
    { linkConfig   :: !LinkConfig
    , createConfig :: !(Maybe Y.Object)
    } deriving stock (Generic, Show, Eq)

instance Y.FromJSON DefaultsConfig

data LinkConfig = LinkConfig
    { create   :: !(Maybe Bool)    -- ^ Create parent dirs (default: false)
    , path     :: !(Maybe String)  -- ^ The source of the link (default: false)
    , relink   :: !(Maybe Bool)    -- ^ Remove target symlink (default: false)
    , force    :: !(Maybe Bool)    -- ^ Force removal of target (default: false)
    , relative :: !(Maybe Bool)    -- ^ Relative pathS to source (default: false)
    } deriving stock (Generic, Show, Eq)

instance Y.FromJSON LinkConfig

-- | Strict Link with no Maybe values
data StrictLink = StrictLink
    { createS   :: !Bool    -- ^ Create parent dirs (default: false)
    , pathS     :: !FilePath  -- ^ The source of the link (default: false)
    , relinkS   :: !Bool    -- ^ Remove target symlink (default: false)
    , forceS    :: !Bool    -- ^ Force removal of target (default: false)
    , relativeS :: !Bool    -- ^ Relative pathS to source (default: false)
    } deriving stock (Generic, Show)

-- | This should never be used outside of testing
empty :: ConfigObj
empty = ConfigObj
    { defaults = DefaultsConfig
        { linkConfig = LinkConfig
            { create   = Just False
            , path     = Just ""
            , relink   = Just False
            , force    = Just False
            , relative = Just False
            }
            , createConfig = Nothing
        }
    , link = HM.empty :: HM.HashMap Text MaybeLinkCfg
    , dirs = Just []
    }

-- | Build a link from a string representing the pathS and a LinkConfig object.
-- TODOMAYBE: Empty string to createS pathS to "./filename", this could be
-- implemented in a different way
buildLinkCfg :: String -> LinkConfig -> LinkConfig
buildLinkCfg str cfg = LinkConfig
    { create   = create cfg
    , path     = Just str
    , relink   = relink cfg
    , force    = force cfg
    , relative = relative cfg
    }

-- | Combine two Links where values in the first that are Nothing are replaced
-- with the value in the second.
-- Uses Data.HashMap.Strict.unionWith.
weightedUnion :: LinkConfig -> LinkConfig -> LinkConfig
weightedUnion x y = LinkConfig
    { create   = leftIfMaybe (create x) (create y)
    , path     = leftIfMaybe (path x) (path y)
    , relink   = leftIfMaybe (relink x) (relink y)
    , force    = leftIfMaybe (force x) (force y)
    , relative = leftIfMaybe (relative x) (relative y)
    }
  where leftIfMaybe v1 v2 = if isJust v1 then v1 else v2

removeMaybes :: LinkConfig -> StrictLink
removeMaybes x = StrictLink
    { createS = fromMaybe False $ create x
    , pathS = decodeString . fromMaybe "" $ path x
    , relinkS = fromMaybe False $ relink x
    , forceS = fromMaybe False $ force x
    , relativeS = fromMaybe False $ relative x
    }

-- TODO: Parse DefaultsConfig object to fill missing values with defaults
-- | Parse config file into Config object
parseConfig :: String -> IO ConfigObj
parseConfig pth = Y.decodeFileThrow pth >>= \res -> return (res :: ConfigObj)

