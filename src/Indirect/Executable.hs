{-# LANGUAGE QuasiQuotes #-}

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
  , doesExecutableExist
  , getTargetsDir
  ) where

import Indirect.Prelude

import Data.Map.Strict qualified as Map
import Indirect.Config (Config (..), Executable (..))
import Indirect.Logging
import Path ((</>), reldir)
import Path.IO
  ( doesFileExist
  , executable
  , getPermissions
  , withCurrentDir
  , withSystemTempDir
  , getXdgDir
  , XdgDirectory(..)
  )
import System.Exit (die)
import System.Process.Typed (proc, runProcess_)

findExecutable :: Config -> String -> IO (Maybe (Path Abs File))
findExecutable config pgname = do
  for (Map.lookup pgname config.unwrap) $ \exe -> do
    exists <- doesExecutableExist exe
    unless exists $ installExecutable pgname exe
    pure exe.binary

installExecutable :: String -> Executable -> IO ()
installExecutable pgname exe = do
  for_ exe.install $ \install -> do
    logInfo $ "Installing " <> highlightFile magenta exe.binary

    withSystemTempDir "indirect.install" $ \tmp ->
      withCurrentDir tmp $ runProcess_ $ proc "sh" ["-c", install]

    created <- doesExecutableFileExist exe.binary
    unless created
      $ die
      $ "install script for "
      <> pgname
      <> " did not create "
      <> toFilePath exe.binary

doesExecutableExist :: Executable -> IO Bool
doesExecutableExist = doesExecutableFileExist . (.binary)

doesExecutableFileExist :: Path Abs File -> IO Bool
doesExecutableFileExist path = do
  exists <- doesFileExist path

  if exists
    then executable <$> getPermissions path
    else pure False

getTargetsDir :: IO (Path Abs Dir)
getTargetsDir = do
  xdg <- getXdgDir XdgData $ Just [reldir|indirect|]
  pure $ xdg </> [reldir|targets|]
