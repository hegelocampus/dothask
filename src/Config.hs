{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Config (parseConfig) where

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..))
import Data.HashMap.Strict as HM
import GHC.Generics
import Turtle hiding (relative)
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

-- | Clean target symlink if needed
cleanTargetLink :: FilePath -> IO ()
cleanTargetLink pth
  | tstpth && isNotSymbolicLink pth = ioError (
      format ("File already exists at:"%fp%"!\n") pth)
  | tstpth = rm pth >> printf ("Removing existing symlink: "%fp%"\n") pth
  | otherwise = printf ("Target "%fp%" is already clean.\n") pth
  where tstpth = testfile pth

-- | Clean target file if needed
cleanTargetFile :: FilePath -> IO ()
cleanTargetFile pth
  | testfile pth = rm pth >> printf ("Removing existing file: "%fp%"\n") pth
  | otherwise = printf ("Target "%fp%" is clean.\n") pth

cleanTarget :: Bool -> Bool -> FilePath -> IO ()
cleanTarget True _ pth = cleanTargetFile pth
cleanTarget _ True pth = cleanTargetLink pth
cleanTarget _ _ pth    = printf ("Assuming target path is clean: "%fp%"\n") pth

-- | Check that the tree exists and if it can be created
checkTree :: FilePath -> Bool -> IO ()
checkTree pth canMake
  | canMake = mktree dirpath >> printf ("Created directory: "%fp%"\n") dirpath
  | testdir dirpath = printf (fp%" already exists!\n") pth
  | otherwise = ioError $ format ("Directory "%fp%" does not exist!\n") dirpath
  where dirpath = dropExtension pth

-- NOTE: symlink fails if the file already exists

-- | Build symlink for LinkConfig datatype
-- | Need to negotiate LinkConfig w/ set defaults
-- | This Function will raise an IO error if it is unable to create the
-- | symlink, this exception should !not! disrupt the execution of the
-- | remainder of the symlinks.
buildLink :: FilePath -> LinkConfig -> IO ()
buildLink pth LinkConfig {create = c, path = src, relink = rln, force = f,
                           relative = rel}
  =  do
    -- Check that dirtree exits
    checkTree pth c
    -- Handle existing files
    cleanTarget f rln pth
    symlink src pth

-- | Parse config file into Config object
parseConfig :: String -> IO Config
parseConfig p = do
  -- res is the type of (MonadIO FromJSON)
  res <- Y.decodeFileThrow p
  return (res :: Config)

