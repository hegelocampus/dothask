{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad as CM
import System.Posix.User (getRealUserID)
import Options.Applicative
import Turtle (die, printf)
import Dothask (buildDots)

data Params = Params
  { configPath  :: String
  , quiet    :: Bool}

params :: Parser Params
params = Params
  <$>  strOption
      ( long "config"
     <> short 'c'
     <> metavar "STRING"
     <> value "./dot.config.yaml"
     <> help "Custom config file path (default is './dot.config.yaml')" )
  <*> switch
      ( long "quiet"
     <> short 'q'
     <> help "Whether to be quiet" )

main :: IO ()
main = noSudo >> execParser opts >>= \p -> buildDots (configPath p) (quiet p)
  where
    opts = info (helper <*> params)
      ( fullDesc
      <> progDesc "Automatically create symlinks for configuration files"
      <> header "dothask - a dotfile setup automation tool" )

noSudo :: IO ()
noSudo = getRealUserID >>= \uId -> when (show uId == "0")
    $ printf sudoWarn >> die "You may not run this command as sudo!"
  where
        sudoWarn = "I appreciate that you trust me enough to grant me \
                \superuser permissions, \n\
                \but please run this program will not run with sudo. \n\
                \If the program is unable to succeed without superuser \
                \permissions, please check your file permissions. \n"

