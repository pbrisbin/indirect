{-# OPTIONS_GHC -Wno-orphans #-}

module Indirect.Orphans () where

import Prelude

import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Map.Monoidal.Strict qualified as MonoidalMap
import Data.Map.Strict qualified as Map
import Data.String (IsString (..))
import Data.Text (unpack)
import TOML

instance (IsString k, Ord k, DecodeTOML v) => DecodeTOML (MonoidalMap k v) where
  tomlDecoder =
    makeDecoder $ \case
      Table o ->
        MonoidalMap.fromList
          . Map.toList
          . Map.mapKeys (fromString . unpack)
          <$> mapM (runDecoder tomlDecoder) o
      v -> typeMismatch v
