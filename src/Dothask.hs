{- |
Copyright: (c) 2020 Bee Ellis
SPDX-License-Identifier: MIT
Maintainer: Bee Ellis <pizzaman8099@gmail.com>

Dotfile setup automation written in Haskell
-}

module Dothask (set_dots) where

import Config


set_dots :: IO ()
set_dots = do
  parseConfig

