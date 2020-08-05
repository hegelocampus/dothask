{- |
Copyright: (c) 2020 Bee Ellis
SPDX-License-Identifier: MIT
Maintainer: Bee Ellis <bellis8099@gmail.com>

Dotfile setup automation written in Haskell
-}

{-# LANGUAGE OverloadedStrings #-}

module Dothask (buildDots, module Dothask.Config) where

import qualified Data.Text as T
--import Data.Yaml ((.:))
import qualified Data.HashMap.Strict as HM
import Control.Monad as CM
import Data.Maybe
import Turtle hiding (relative, x)
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
buildDots configPath noConfirm = do
    ConfigObj { defaults = cfg, link = lnks } <- parseConfig configPath
    usrHome <- home
    curpwd <- pwd
    mapM_ (\(trg, lnk) ->
          makeLink curpwd (parsePath usrHome trg) noConfirm
          $ setDefaults (linkConfig cfg) lnk
          ) $ HM.toList lnks

-- | Fill the default values for the LinkConfig object based on the set config
-- values if avaliable, otherwise use default values.
setDefaults :: LinkConfig -> MaybeLinkCfg -> StrictLink
setDefaults cfg lnk
    | isJust lnk = removeMaybes $ weightedUnion (fromJust lnk) cfg
    | otherwise = removeMaybes $ buildWCfg ""
  where
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
-- TODO: Add param that causes link creation failure to raise an
-- error and break from the iteration.
makeLink :: FilePath -> FilePath -> Bool -> StrictLink -> IO ()
makeLink curpwd pth noConfirm StrictLink
    { createS = c
    , pathS = src
    , forceS = frc
    , relinkS = rln
    --, relativeS = rel
    }
    = do
        printf ("Attempting to link "%s%"\n") lnkTxt
        dExists <- checkTree (directory pth) c
        tClean <- cleanTarget frc rln noConfirm pth
        if dExists && tClean
           then symlink fullSrc pth
           else handleBadLink pth lnkTxt (not tClean)
  where
      lnkTxt = format (fp%" to "%fp) fullSrc pth
      fullSrc = curpwd </> srcFile
      srcFile = if filename src == "" then src </> fileWithoutDot else src
      fileWithoutDot = decodeString . tail . encodeString $ filename pth

handleBadLink :: FilePath -> Text -> Bool -> IO()
handleBadLink pth lnkTxt fpError =
    eprintf ("Could not create link from "%s%"!\n") lnkTxt
    >> if fpError
          then eprintf ("Filepath is not clean!\n"%fp%" already exists!\n\n") pth
          else eprintf "Containing directory could not be created!\n\n"

-- | Create error message from Format.
formatForError :: Format Text (a -> Text) -> a -> String
formatForError txt = T.unpack . format txt

-- | Clean target if allowed, raise error if file exists and cleaning is not allowed.
cleanTarget :: Bool -> Bool -> Bool -> FilePath -> IO Bool
cleanTarget True _ noConfirm pth = cleanTargetFile pth noConfirm
cleanTarget _ True _ pth         = cleanTargetLink pth
cleanTarget _ _ _ pth            = not <$> testfile pth

-- | Clean target file if needed, return true if filepath was able to be cleaned.
cleanTargetFile :: FilePath -> Bool -> IO Bool
cleanTargetFile pth noConfirm = testfile pth >>= \isFile ->
    if isFile
        then if noConfirm
            then removeFile "file" pth
            else requireConfirm (removeFile "file") pth
        else return True

-- | Clean target symlink if needed.
cleanTargetLink :: FilePath -> IO Bool
cleanTargetLink pth = testfile pth >>= \isFile ->
    if isFile
        then isNotSymbolicLink pth >>= \notLn -> if notLn
            then return False
            else removeFile "symlink" pth
        else return True

removeFile :: Text -> FilePath -> IO Bool
removeFile fType pth = printf ("Removing existing " %s% ": '" % fp % "'...\n") fType pth
                 >> rm pth >> return True


requireConfirm :: (FilePath -> IO Bool) -> FilePath  -> IO Bool
requireConfirm fileOp pth = do
    printf ("Are you sure you want to overwrite '" % fp % "'?\n") pth
    res <- getLine
    if res == "y" || res == "ye" || res == "yes"
       then fileOp pth
    else return False

-- | Check that the tree exists and if it can be created.
checkTree :: FilePath -> Bool -> IO Bool
checkTree pth True = mktree pth
                     >> printf ("Created directory: "%fp%"\n") pth
                     >> return True
checkTree pth _    = testdir pth

