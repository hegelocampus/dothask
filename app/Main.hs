module Main (main) where

import Options.Applicative
--import Data.Semigroup ((<>))
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
main = execParser opts >>= \p -> buildDots (configPath p) (quiet p)
  where
    opts = info (helper <*> params)
      ( fullDesc
      <> progDesc "Automatically create symlinks for configuration files"
      <> header "dothask - a dotfile setup automation tool" )

