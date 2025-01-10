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
  , forEachExecutable_
  ) where

import Indirect.Prelude

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

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

forEachExecutable_
  :: Applicative m
  => Config
  -> [String]
  -> (Executable -> m ())
  -> m ()
forEachExecutable_ config only f =
  for_ (Map.toList $ config.unwrap) $ \(name, exe) -> do
    when (maybe True (name `elem`) $ nonEmpty only)
      $ f exe
