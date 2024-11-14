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
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.FilePath (takeDirectory, (<.>), (</>))

loadConfig :: IO Config
loadConfig = do
  user <- getUserConfig
  proj <- getProjectConfig
  raw <-
    (<>)
      <$> loadRawConfig user
      <*> maybe (pure mempty) loadRawConfig proj

  resolveConfig raw

getUserConfig :: IO FilePath
getUserConfig = do
  xdg <- getUserConfigDir "indirect"
  pure $ xdg </> "indirect" <.> "toml"

getProjectConfig :: IO (Maybe FilePath)
getProjectConfig = locateInParents $ ".indirect" <.> "toml"

locateInParents :: FilePath -> IO (Maybe FilePath)
locateInParents path = go =<< getCurrentDirectory
 where
  go cwd = do
    let absPath = cwd </> path

    exists <- doesFileExist absPath

    if exists
      then pure $ Just absPath
      else if cwd == "/" then pure Nothing else go $ takeDirectory cwd
