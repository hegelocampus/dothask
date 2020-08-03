{- |
Copyright: (c) 2020 Bee Ellis
SPDX-License-Identifier: MIT
Maintainer: Bee Ellis <bellis8099@gmail.com>

Dotfile setup automation written in Haskell
-}

{-# LANGUAGE OverloadedStrings #-}

module Dothask (
      buildDots
    , module Dothask.Config
    ) where

import qualified Data.Text as T
--import Data.Yaml ((.:))
import qualified Data.HashMap.Strict as HM
import Control.Monad as CM
import Data.Maybe
import Turtle hiding (relative)
import Prelude hiding (FilePath)

import Dothask.Config
    ( parseConfig
    , removeMaybes
    , buildLinkCfg
    , weightedUnion
    , MaybeLinkCfg
    , DefaultsConfig (..)
    , LinkConfig (..)
    , ConfigObj (..)
    , StrictLink (..)
    )

-- | Takes in config path, parses config yaml file at that path, contructing softlinks for each etries.
buildDots :: String -> Bool -> IO ()
buildDots configPath _ = do
    ConfigObj { defaults = cfg, link = lnks } <- parseConfig configPath
    usrHome <- home
    curpwd <- pwd
    mapM_ (\(trg, lnk) ->
          makeLink curpwd (parsePath usrHome trg) $ setDefaults (linkConfig cfg) lnk
          ) $ HM.toList lnks

-- | Fill the default values for the LinkConfig object based on the set config
-- values if avaliable, otherwise use default values.
setDefaults :: LinkConfig -> MaybeLinkCfg -> StrictLink
setDefaults cfg lnk
    | isJust lnk = removeMaybes . unionCfg $ fromJust lnk
    | otherwise = removeMaybes $ buildWCfg ""
  where
      unionCfg x    = weightedUnion x cfg
      buildWCfg src = buildLinkCfg src cfg

-- | If there is a leading tilde replace it with the users $HOME path,
-- else return the path as it stands
parsePath :: FilePath -> Text -> FilePath
parsePath usrHome txtPth = maybe pth (usrHome </>) filep
  where filep = stripPrefix "~/" pth
        pth = fromText txtPth

-- TODO: Need to negotiate LinkConfig w/ set defaults
-- | Build symlink for LinkConfig datatype. This Function will raise an
-- IO error if it is unable to create the symlink given the LinkConfig's
-- options.
-- TODO: Remove exceptions, change checkTree to return bool indicating if directory exits or it was created. Only preceed onto clean target if true. Same for cleanTarget
makeLink :: FilePath -> FilePath -> StrictLink -> IO ()
makeLink curpwd pth StrictLink
    { createS = c
    , pathS = src
    , forceS = frc
    , relinkS = rln
    , relativeS = rel
    }
    = printf ("Attempting to link "%fp%" to "%fp%"\n") fullSrc pth >>
      checkTree (directory pth) c >>
      cleanTarget frc rln pth >>
      symlink fullSrc pth
  where
      fullSrc = curpwd </> filep
      filep = if filename src == "" then src </> fileWithoutDot else src
      fileWithoutDot = decodeString . tail . encodeString $ filename pth

-- | Create error message from Format.
formatForError :: Format Text (a -> Text) -> a -> String
formatForError txt = T.unpack . format txt

-- | Clean target symlink if needed.
cleanTargetLink :: FilePath -> IO ()
cleanTargetLink pth = testfile pth >>= \isFile ->
    CM.when isFile $ isNotSymbolicLink pth >>= \isLn -> if isLn
        then error $ formatForError ("Will not remove regular file at: " % fp % "\n") pth
        else printf ("Removing existing symlink: " % fp % "\n") pth >> rm pth

-- | Clean target file if needed.
-- TODO: Add printline that asks user to confirm that they want to delete the
-- existing file. It would probably be most clear to deligate this task to
-- a helper funciton.
cleanTargetFile :: FilePath -> IO ()
cleanTargetFile pth = testfile pth >>= \isFile ->
    CM.when isFile $ printf ("Removing existing file: '"%fp%"'...\n") pth >> rm pth

-- | Clean target if allowed, raise error if file exists and cleaning is not allowed.
-- TODO: Require user input to overwrite existing file usless passed a special
-- "--do-not-ask" flag.
cleanTarget :: Bool -> Bool -> FilePath -> IO ()
cleanTarget True _ pth = cleanTargetFile pth
cleanTarget _ True pth = cleanTargetLink pth
cleanTarget _ _ pth    = testfile pth >>= \exists ->
    CM.when exists . error $ errorMsg pth
  where errorMsg = formatForError ("Filepath is not clean!\n"%fp%" already exists!\n")

-- | Check that the tree exists and if it can be created.
checkTree :: FilePath -> Bool -> IO ()
checkTree pth True = mktree pth >> printf ("Created directory: "%fp%"\n") pth
checkTree pth _ = testdir pth >>= \exists -> if exists
    then printf ("Do not need to create containing directory: "%fp%"\n") pth
    else error $ formatForError ("Directory "%fp%" does not exist!\n") pth

