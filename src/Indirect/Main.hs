module Indirect.Main
  ( main
  , mainAs
  ) where

import Indirect.Prelude

import Indirect.CLI qualified as CLI
import Indirect.Config qualified as Config
import Indirect.Executable
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith)
import System.Process.Typed (proc, runProcess)

main :: IO ()
main = mainAs =<< getProgName

mainAs :: String -> IO ()
mainAs progName = do
  config <- Config.load
  findExecutable config progName >>= \case
    Nothing -> CLI.run config
    Just exe -> do
      args <- getArgs
      ec <- runProcess $ proc (toFilePath exe) args
      exitWith ec
