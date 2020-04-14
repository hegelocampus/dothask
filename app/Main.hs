module Main (main) where

import Options.Applicative
import Data.Semigroup ((<>))
import Dothask (build_dots)

data Params = Params
  { optQuiet    :: Bool
  , configPath  :: String }

params :: Parser Params
params = Params
  <$> argument str
      ( metavar "STRING"
      <> help "Custom config file path (default is './dot.config.yaml')" )
  <*> switch
      ( long "quiet"
      <> short 'q'
      <> help "Set quiet mode" )

params :: Parser Params

main :: IO ()
main = execParser opts >>= build_dots
  where
    opts = info (helper <*> params)
      ( fullDesc
      <> progDesc "Automatically create symlinks for configuration files"
      <> header "dothask - a dotfile setup automation tool" )
