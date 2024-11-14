-- |
--
-- Module      : Indirect.Executable
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Indirect.Executable
  ( findExecutable
  , installExecutable
  ) where

import Indirect.Prelude

import Data.Map.Strict qualified as Map
import Indirect.Config (Config (..), Executable (..))
import Indirect.Logging
import Path.IO (doesFileExist, executable, getPermissions)
import System.Exit (die)
import System.Process.Typed (proc, runProcess_)

findExecutable :: Config -> String -> IO (Maybe (Path Abs File))
findExecutable config pgname = do
  for (Map.lookup pgname config.unwrap) $ \exe -> do
    installExecutable pgname exe
    pure exe.binary

installExecutable :: String -> Executable -> IO ()
installExecutable pgname exe = do
  exists <- doesExecutableFileExist exe.binary

  let mInstall = do
        guard $ not exists
        exe.install

  for_ mInstall $ \install -> do
    logInfo $ "Installing " <> toFilePath exe.binary
    runProcess_ $ proc "sh" ["-c", install]
    created <- doesExecutableFileExist exe.binary
    unless created
      $ die
      $ "install script for "
      <> pgname
      <> " did not create "
      <> toFilePath exe.binary

doesExecutableFileExist :: Path Abs File -> IO Bool
doesExecutableFileExist path = do
  exists <- doesFileExist path

  if exists
    then executable <$> getPermissions path
    else pure False
