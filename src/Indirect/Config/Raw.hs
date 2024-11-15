-- |
--
-- Module      : Indirect.Config.Raw
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Indirect.Config.Raw
  ( RawConfig (..)
  , RawExecutable (..)
  , loadRawConfig
  ) where

import Indirect.Prelude

import Control.Exception (throwIO)
import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Map.Monoidal.Strict qualified as MonoidalMap
import Data.Semigroup.Generic
import Path.IO (doesFileExist)
import TOML

newtype RawConfig = RawConfig
  { unwrap :: MonoidalMap String RawExecutable
  }
  deriving newtype (Semigroup, Monoid, DecodeTOML)

data RawExecutable = RawExecutable
  { vars :: MonoidalMap Text (Last Text)
  , binary :: Maybe (Last Text)
  , install :: Maybe (Last Text)
  }
  deriving stock Generic
  deriving (Semigroup, Monoid) via (GenericSemigroupMonoid RawExecutable)

instance DecodeTOML RawExecutable where
  tomlDecoder =
    RawExecutable
      <$> getFieldOr mempty "vars"
      <*> getFieldOpt "binary"
      <*> getFieldOpt "install"

loadRawConfig :: Path Abs File -> IO RawConfig
loadRawConfig path = do
  exists <- doesFileExist path
  if exists
    then do
      result <- TOML.decodeFile $ toFilePath path
      either throwIO pure result
    else pure mempty
