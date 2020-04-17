{- |
Copyright: (c) 2020 Bee Ellis
SPDX-License-Identifier: MIT
Maintainer: Bee Ellis <bellis8099@gmail.com>

Dotfile setup automation written in Haskell
-}

{-# LANGUAGE OverloadedStrings #-}

module Dothask (buildDots) where

import Data.Yaml ((.:))
import Turtle
import Config

-- Actually build the Symlinks
--buildLinks :: Config link -> IO ()
--buildLinks links = mapM (/lnk -> symLink $

buildDots :: String -> Bool -> IO ()
buildDots configPath isQuiet = do
  res <- parseConfig configPath
  print (res)


