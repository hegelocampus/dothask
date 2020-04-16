module Main (main) where

import Options.Applicative
import Data.Semigroup ((<>))
import Dothask (buildDots)

data Params = Params
  { configPath  :: String
  , optQuiet    :: Bool}

params :: Parser Params
params = Params
  <$> argument str
      ( metavar "STRING"
      <> help "Custom config file path (default is './dot.config.yaml')" )
  <*> switch
      ( long "quiet"
      <> short 'q'
      <> help "Set quiet mode" )

-- Config path doesn't rely on the config file so it can be set early
buildDotsWrapper :: Params -> IO ()
buildDotsWrapper (Params configPath optQuiet)
  | length configPath > 0 = buildDots path optQuiet
  | otherwise             = buildDots defPath optQuiet
  where defPath = "./dot.config.yaml"

main :: IO ()
main = execParser opts >>= buildDotsWrapper
  where
    opts = info (helper <*> params)
      ( fullDesc
      <> progDesc "Automatically create symlinks for configuration files"
      <> header "dothask - a dotfile setup automation tool" )
