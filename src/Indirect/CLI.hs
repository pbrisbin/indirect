-- |
--
-- Module      : Indirect.CLI
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Indirect.CLI
  ( run
  ) where

import Indirect.Prelude

import Data.Map.Strict qualified as Map
import Data.Text.Escaped
import Data.Text.IO qualified as T
import Indirect.Config (Config (..))
import Indirect.Executable
import Indirect.Logging
import Indirect.Options
import Path (filename, parent, parseAbsFile)
import Path.IO (doesFileExist, withCurrentDir)
import System.Environment (getExecutablePath)

run :: Config -> IO ()
run config = do
  options <- parseOptions
  renderer <- terminalRenderer
  self <- parseAbsFile =<< getExecutablePath

  let
    bin = parent self
    indirect = filename self

    forExes only f =
      for_ (Map.toList $ config.unwrap) $ \(name, exe) -> do
        when (maybe True (name `elem`) $ nonEmpty only)
          $ f exe

  withCurrentDir bin $ case options.command of
    List loptions -> do
      forExes loptions.only $ \exe -> do
        linkExists <- doesFileExist exe.link
        execExists <- doesExecutableFileExist exe.binaryAbs

        T.putStrLn
          $ renderer
          $ highlightLinkName exe.link
          <> " -> "
          <> highlightLinkTarget exe.binaryAbs
          <> (if linkExists then "" else red " (missing link)")
          <> (if execExists then "" else yellow " (needs install)")
    Setup soptions -> do
      forExes soptions.only $ \exe -> do
        linkExists <- doesFileExist exe.link
        execExists <- doesExecutableFileExist exe.binaryAbs

        let
          needsLink = exe.link /= indirect && (not linkExists || soptions.force)
          needsInstall = soptions.install && (not execExists || soptions.force)

        when needsLink $ linkExecutable indirect exe
        when needsInstall $ installExecutable exe
    Clean coptions -> do
      forExes coptions.only $ \exe -> do
        linkExists <- doesFileExist exe.link
        execExists <- doesExecutableFileExist exe.binaryAbs
        when linkExists $ unlinkExecutable exe
        when execExists $ removeExecutable exe
