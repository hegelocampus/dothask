{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Config (parseConfig) where

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..))
import Data.HashMap.Strict as HM
import GHC.Generics
import System.Directory (pathIsSymbolicLink, doesPathExist)
import Turtle
import Prelude hiding (FilePath)

data LinkConfig =
  LinkConfig {
    create   :: Bool -- Create parent directories up to target (default: null)
  , path     :: FilePath -- The source of the symlink (default: false)
  , relink   :: Bool -- Remove the old target if it is a symlink (default: false)
  , force    :: Bool -- Force removal of old target and create new link (default: false)
  , relative :: Bool -- Use relative path to the source (default: false, absolute links)
  } deriving (Generic, Show)

instance FromJSON LinkConfig

-- TODOMAYBE: Move type definitions to their own files to facilitate use
data Config =
  Config {
    defaults  :: Y.Object
  , link      :: HM.HashMap FilePath (Maybe LinkConfig)
  } deriving (Generic, Show)

instance FromJSON Config

-- TODO: Create setDefaults function
-- setDefaults should use Data.Yaml (.!=) to set missing default values

-- | Clean target if needed
cleanTarget :: FilePath -> IO ()
cleanTarget pth
  | testfile pth = rm pth >> stdout ("Removing " ++ pth)
  | otherwise = stdout ("Target " ++ pth ++ " is already clean.")

-- | Build symlink for LinkConfig datatype
-- | Need to negotiate LinkConfig w/ set defaults
buildLink :: FilePath -> LinkConfig -> IO ()
buildLink trg (LinkConfig {create = c, path = src, relink = rln, force = f, relative = rel})
  | (not rln) && (doesPathExist trg) && (pathIsSymbolicLink) = -- Print link already exist
  | (not f) && (doesPathExist trg) = -- Return IO error
  | otherwise = do
    if c
       then mktree $ directory src >> buildLink 
       else if (
    -- symlink fails if the file already exists
    symLink $

-- | Parse config file into Config object
parseConfig :: String -> IO Config
parseConfig p = do
  -- res is the type of (MonadIO FromJSON)
  res <- Y.decodeFileThrow p
  return (res :: Config)

