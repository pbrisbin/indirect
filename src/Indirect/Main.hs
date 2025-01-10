-- |
--
-- Module      : Indirect.Main
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Indirect.Main
  ( main
  ) where

import Indirect.Prelude

import Indirect.CLI qualified as CLI
import Indirect.Config.Load
import Indirect.Executable
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith)
import System.Process.Typed (proc, runProcess)

main :: IO ()
main = do
  config <- loadConfig
  pgname <- getProgName
  findExecutable True config pgname >>= \case
    Nothing -> CLI.run config
    Just exe -> do
      args <- getArgs
      ec <- runProcess $ proc (toFilePath exe) args
      exitWith ec
