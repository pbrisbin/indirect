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
  { binary :: Path Abs File
  , install :: Maybe String
  }
  deriving stock (Show, Eq)
