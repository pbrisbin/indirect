-- |
--
-- Module      : Indirect.Executable
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Indirect.Executable
  ( Executable (..)
  , findExecutable
  , installExecutable
  , removeExecutable
  , linkExecutable
  , unlinkExecutable
  ) where

import Indirect.Prelude

import Data.Map.Strict qualified as Map
import Indirect.Config (Config (..), Executable (..))
import Indirect.Logging
import Path (parent)
import Path.IO
  ( createFileLink
  , doesFileExist
  , ensureDir
  , removeFile
  , withCurrentDir
  , withSystemTempDir
  )
import System.Process.Typed (proc, runProcess_)

findExecutable :: Config -> String -> Maybe Executable
findExecutable config pgname = Map.lookup pgname config.unwrap

installExecutable :: Executable -> IO ()
installExecutable exe = do
  let target = exe.binaryAbs
  ensureDir $ parent target
  logInfo $ "Installing " <> highlightLinkTarget target

  withSystemTempDir "indirect.install" $ \tmp -> do
    withCurrentDir tmp $ do
      runProcess_ $ proc "sh" ["-c", exe.install, "--", toFilePath target]
      created <- doesExecutableFileExist target
      unless created
        $ die
        $ "install script for "
        <> highlightLinkName exe.link
        <> " did not create "
        <> highlightLinkTarget target

removeExecutable :: Executable -> IO ()
removeExecutable exe = do
  logInfo $ "Removing " <> highlightLinkTarget exe.binaryAbs
  removeFile exe.binaryAbs

linkExecutable :: Path Rel File -> Executable -> IO ()
linkExecutable indirect exe = do
  logInfo
    $ "Linking "
    <> highlightLinkName exe.link
    <> " => "
    <> highlightLinkTarget indirect

  linkExists <- doesFileExist exe.link
  when linkExists $ removeFile exe.link
  createFileLink indirect exe.link

unlinkExecutable :: Executable -> IO ()
unlinkExecutable exe = do
  logInfo $ "Removing " <> highlightLinkName exe.link
  removeFile exe.link
