module Indirect.Main
  ( main
  ) where

import Indirect.Prelude

import Indirect.CLI qualified as CLI
import Indirect.Config qualified as Config
import Indirect.Executable
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith)
import System.Process.Typed (proc, runProcess)

main :: IO ()
main = do
  config <- Config.load
  pgname <- getProgName
  findExecutable config pgname >>= \case
    Nothing -> CLI.run
    Just exe -> do
      args <- getArgs
      ec <- runProcess $ proc (toFilePath exe) args
      exitWith ec
