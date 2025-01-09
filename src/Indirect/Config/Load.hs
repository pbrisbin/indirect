{-# LANGUAGE QuasiQuotes #-}

-- |
--
-- Module      : Indirect.Config.Load
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Indirect.Config.Load
  ( loadConfig
  ) where

import Indirect.Prelude

import Indirect.Config
import Indirect.Config.Raw
import Indirect.Config.Resolve
import Path (absdir, reldir, relfile, (</>))
import Path qualified
import Path.IO (XdgDirectory (..), doesFileExist, getCurrentDir, getXdgDir)

loadConfig :: IO Config
loadConfig = do
  user <- getUserConfig
  proj <- getProjectConfig
  raw <-
    (<>)
      <$> loadRawConfig user
      <*> maybe (pure mempty) loadRawConfig proj

  resolveConfig raw

getUserConfig :: IO (Path Abs File)
getUserConfig = do
  xdg <- getXdgDir XdgConfig $ Just [reldir|indirect|]
  pure $ xdg </> [relfile|indirect.toml|]

getProjectConfig :: IO (Maybe (Path Abs File))
getProjectConfig = locateInParents [relfile|.indirect.toml|]

locateInParents :: Path Rel File -> IO (Maybe (Path Abs File))
locateInParents path = go =<< getCurrentDir
 where
  go :: Path Abs Dir -> IO (Maybe (Path Abs File))
  go cwd = do
    let absPath = cwd </> path

    exists <- doesFileExist absPath

    if exists
      then pure $ Just absPath
      else if cwd == root then pure Nothing else go $ Path.parent cwd

-- NB. unclear if this works for windows
root :: Path Abs Dir
root = [absdir|/|]
