{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Config
    (
      parseConfig
    , LinkConfig
        (
          create
        , path
        , relink
        , force
        , relative
        )
    , Config
        (
          defaults
        , link
        )
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

data LinkConfig = LinkConfig
  {
    create   :: !Bool      -- ^ Create parent directories up to target (default: null)
  , path     :: !FilePath  -- ^ The source of the symlink (default: false)
  , relink   :: !Bool      -- ^ Remove target if it is a symlink (default: false)
  , force    :: !Bool      -- ^ Force removal of target (default: false)
  , relative :: !Bool      -- ^ Use relative path to source (default: false)
  } deriving stock (Generic, Show)

instance Y.FromJSON LinkConfig

-- TODOMAYBE: Move type definitions to their own files to facilitate use
data Config = Config
  {
    defaults  :: !Y.Object
  , link      :: !(HM.HashMap FilePath (Maybe LinkConfig))
  } deriving stock (Generic, Show)

instance Y.FromJSON Config


-- | Parse config file into Config object
parseConfig :: String -> IO Config
parseConfig p = Y.decodeFileThrow p >>= \res -> return (res :: Config)

