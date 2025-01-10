-- |
--
-- Module      : Indirect.Config.Resolve
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Indirect.Config.Resolve
  ( resolveConfig
  , resolveExecutable
  ) where

import Indirect.Prelude

import Control.Exception (throwIO)
import Data.Map.Monoidal.Strict qualified as MonoidalMap
import Data.Map.Strict qualified as Map
import Data.Semigroup (Endo (..))
import Data.Text qualified as T
import Indirect.Config
import Indirect.Config.Raw
import Path (parseRelFile)
import System.Environment (getEnvironment)
import TOML

resolveConfig :: RawConfig -> IO Config
resolveConfig rc = do
  fmap (Config . Map.fromList . MonoidalMap.toList)
    $ MonoidalMap.traverseWithKey (\k -> resolveExecutable k . (defaults <>))
    $ MonoidalMap.filterWithKey (\k _ -> k /= defaultsKey) rc.unwrap
 where
  defaults :: RawExecutable
  defaults = fromMaybe mempty $ MonoidalMap.lookup defaultsKey rc.unwrap

  defaultsKey :: String
  defaultsKey = "defaults"

resolveExecutable :: String -> RawExecutable -> IO Executable
resolveExecutable name re = do
  env <- map (bimap pack pack) <$> getEnvironment

  let
    vars :: [(Text, Text)]
    vars =
      env
        <> [("name", pack name)]
        <> map (second getLast) (MonoidalMap.toList re.vars)

  binaryT <-
    maybe
      (throwIO $ DecodeError [Key "binary"] MissingField)
      (pure . interpolate vars . getLast)
      re.binary

  let binaryV = ("binary", binaryT)

  install <-
    maybe
      (throwIO $ DecodeError [Key "install"] MissingField)
      (pure . unpack . interpolate (binaryV : vars) . getLast)
      re.install

  Executable
    <$> parseRelFile (unpack binaryT)
    <*> pure install

interpolate :: [(Text, Text)] -> Text -> Text
interpolate vs = f . f -- do it twice so that cross-referencing works
 where
  f :: Text -> Text
  f = appEndo $ foldMap (\(k, v) -> Endo $ T.replace ("${" <> k <> "}") v) vs
