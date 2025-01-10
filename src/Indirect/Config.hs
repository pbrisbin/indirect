-- |
--
-- Module      : Indirect.Config
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Indirect.Config
  ( Config (..)
  , Executable (..)
  ) where

import Indirect.Prelude

import Data.Map.Strict (Map)

newtype Config = Config
  { unwrap :: Map String Executable
  }
  deriving stock (Show, Eq)

data Executable = Executable
  { link :: Path Rel File
  -- ^ e.g. @fourmolu@
  , binaryRel :: Path Rel File
  -- ^ e.g. @fourmolu-0.1.0.0@
  , binaryAbs :: Path Abs File
  -- ^ e.g. @$XDG_DATA_DIR/indirect/targets/fourmolu-0.1.0.0@
  , install :: String
  }
  deriving stock (Show, Eq)
