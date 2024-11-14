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
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.FilePath ((<.>), (</>))

loadConfig :: IO Config
loadConfig = do
  xdg <- getUserConfigDir "indirect"
  raw <-
    (<>)
      <$> loadRawConfig (xdg </> "indirect" <.> "toml")
      <*> loadRawConfig (".indirect" <.> "toml")

  resolveConfig raw
