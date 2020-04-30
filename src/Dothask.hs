{- |
Copyright: (c) 2020 Bee Ellis
SPDX-License-Identifier: MIT
Maintainer: Bee Ellis <bellis8099@gmail.com>

Dotfile setup automation written in Haskell
-}

{-# LANGUAGE OverloadedStrings #-}

module Dothask (buildDots, buildLink) where

import qualified Data.Text as T
--import Data.Yaml ((.:))
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Turtle hiding (relative)
import Prelude hiding (FilePath)

import Config
    ( parseConfig
    , removeMaybes
    , buildLink
    , weightedUnion
    , MaybeLinkCfg
    , DefaultsConfig (..)
    , LinkConfig (..)
    , ConfigObj (..)
    , StrictLink (..)
    )

-- TODO: Create setDefaults function
-- setDefaults should use Data.Yaml (.!=) to set missing default values

-- | Create error message from Format.
raiseErrorF :: Format Text (a -> Text) -> a -> c
raiseErrorF txt = error . T.unpack . format txt

-- | Clean target symlink if needed.
cleanTargetLink :: FilePath -> IO ()
cleanTargetLink pth = testfile pth >>= \tstpth -> if tstpth
    then isNotSymbolicLink pth >>= \isLn -> if isLn
        then raiseErrorF ("File already exists at: " % fp % "\n") pth
        else rm pth >> printf ("Removing existing symlink: " % fp % "\n") pth
    else printf ("Target "%fp%" is already clean.\n") pth

-- | Clean target file if needed.
-- TODO: Add printline that asks user to confirm that they want to delete the
-- existing file. It would probably be most clear to deligate this task to
-- a helper funciton.
cleanTargetFile :: FilePath -> IO ()
cleanTargetFile pth = testfile pth >>= \exists -> if exists
    then rm pth >> printf ("Removing existing file: "%fp%"\n") pth
    else printf ("Target "%fp%" is already clean.\n") pth

-- | Clean target if allowed, raise error if file exists and cleaning is not allowed.
cleanTarget :: Bool -> Bool -> FilePath -> IO ()
cleanTarget True _ pth = cleanTargetFile pth
cleanTarget _ True pth = cleanTargetLink pth
cleanTarget _ _ pth    = testfile pth >>= \exists -> if exists
    then raiseErrorF ("Filepath is not clean!\n"%fp%" already exists!\n") pth
    else printf ("Path is clean: "%fp%"\n") pth

-- | Check that the tree exists and if it can be created.
checkTree :: FilePath -> Bool -> IO ()
checkTree pth True = mktree pth >> printf ("Created directory: "%fp%"\n") pth
checkTree pth _ = testdir pth >>= \exists -> if exists
    then printf (fp%" already exists\n") pth
    else raiseErrorF ("Directory "%fp%" does not exist!\n") pth

-- NOTE: symlink fails if the file already exists

-- TODO: Need to negotiate LinkConfig w/ set defaults
-- | Build symlink for LinkConfig datatype. This Function will raise an
-- IO error if it is unable to create the symlink given the LinkConfig's
-- options.
makeLink :: FilePath -> StrictLink -> IO ()
makeLink pth StrictLink
    { create = c
    , path = src
    , relink = rln
    , force = frc
    , relative = rel
    }
    = checkTree (dropExtension pth) c >>
      cleanTarget frc rln pth >>
      symlink src pth

-- | Fill the default values for the LinkConfig object based on the set config
-- values if avaliable, otherwise use default values.
setDefaults :: LinkConfig -> MaybeLinkCfg -> StrictLink
setDefaults cfg lnk
    | isJust lnk = removeMaybes . either buildWCfg unionCfg $ fromJust lnk
    | otherwise = removeMaybes $ buildWCfg ""
  where
      unionCfg bltLnk = weightedUnion bltLnk cfg
      buildWCfg src = buildLink src cfg

buildDots :: String -> Bool -> IO ()
buildDots configPath _ =
  parseConfig configPath >>= \ConfigObj { defaults = cfg, link = lnks } ->
    -- Pass makeLink a pure StrictLink recond created from Link,
    -- Config settings, and defaults
    mapM_ (\(trg, lnk) ->
            makeLink (fromText trg) $ setDefaults (linkConfig cfg) lnk
        ) $ HM.toList lnks

