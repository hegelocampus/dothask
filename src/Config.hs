{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Config
    (
      parseConfig
    , LinkConfig (..)
    , ConfigObj (..)
    , StrictLink (..)
    ) where

import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as HM
import GHC.Generics

data DefaultsConfig = DefaultsConfig
    { linkConfig   :: !LinkConfig
    , createConfig :: !(Maybe Y.Object)
    } deriving stock (Generic, Show)

instance Y.FromJSON DefaultsConfig

data LinkConfig = LinkConfig
    { create   :: !(Maybe Bool)    -- ^ Create parent dirs (default: false)
    , path     :: !(Maybe String)  -- ^ The source of the link (default: false)
    , relink   :: !(Maybe Bool)    -- ^ Remove target symlink (default: false)
    , force    :: !(Maybe Bool)    -- ^ Force removal of target (default: false)
    , relative :: !(Maybe Bool)    -- ^ Relative path to source (default: false)
    } deriving stock (Generic, Show)

instance Y.FromJSON LinkConfig

-- | Combine two Links where values in the first that are Nothing are replaced
-- with the value in the second.
-- Uses Data.HashMap.Strict.unionWith.
union :: LinkConfig -> LinkConfig -> LinkConfig
union (c, p, r, f, rel) (c2, p2, r2, f2, rel) = HM.unionWith clearLNothings
    where clearLNothings x y = if isJust x then x else y

unionS :: LinkConfig -> StrictLink -> StrictLink
unionS = HM.unionWith setPure
    where setPure cv sv = fromMaybe sv cv

data StrictLink = StrictLink
    { createCfg   :: !Bool    -- ^ Create parent dirs (default: false)
    , pathCfg     :: !String  -- ^ The source of the link (default: false)
    , relinkCfg   :: !Bool    -- ^ Remove target symlink (default: false)
    , forceCfg    :: !Bool    -- ^ Force removal of target (default: false)
    , relativeCfg :: !Bool    -- ^ Relative path to source (default: false)
    } deriving stock (Generic, Show)

-- | TODO Set defaults to be Maybe DefaultsConfig.
data ConfigObj = ConfigObj
    { defaults :: !DefaultsConfig                                 -- ^ Defaults config object
    -- Order of operations
    , link     :: !(HM.HashMap FilePath (Maybe (Either String LinkConfig)))  -- ^ Link configuration
    } deriving stock (Generic, Show)

instance Y.FromJSON ConfigObj

-- TODO: Parse DefaultsConfig object to fill missing values with defaults
-- | Parse config file into Config object
parseConfig :: String -> IO ConfigObj
parseConfig p = Y.decodeFileThrow p >>= \res -> return (res :: ConfigObj)

