{- |
Copyright: (c) 2020 Bee Ellis
SPDX-License-Identifier: MIT
Maintainer: Bee Ellis <pizzaman8099@gmail.com>

Dotfile setup automation written in Haskell
-}

module Dothask (build_dots) where

import Config


build_dots :: IO ()
build_dots = do
  res <- parseConfig "dot.config.yaml" -- This should be passed in
  print res

