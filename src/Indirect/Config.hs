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
  , load
  ) where

import Indirect.Prelude

import Control.Exception (throwIO)
import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Map.Monoidal.Strict qualified as MonoidalMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup (Endo (..))
import Data.Semigroup.Generic
import Data.Text qualified as T
import Path (parseAbsFile)
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.FilePath ((<.>), (</>))
import TOML

newtype Config = Config
  { unwrap :: Map String Executable
  }

data Executable = Executable
  { binary :: Path Abs File
  , install :: Maybe String
  }

load :: IO Config
load = do
  xdg <- getUserConfigDir "indirect"
  raw <-
    (<>)
      <$> loadRawConfig (xdg </> "indirect" <.> "toml")
      <*> loadRawConfig (".indirect" <.> "toml")

  resolveConfig raw

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

  let
    binaryV = ("binary", binaryT)
    install = unpack . interpolate (binaryV : vars) . getLast <$> re.install

  Executable
    <$> parseAbsFile (unpack binaryT)
    <*> pure install

interpolate :: [(Text, Text)] -> Text -> Text
interpolate vs = f . f -- do it twice so that cross-referencing works
 where
  f :: Text -> Text
  f = appEndo $ foldMap (\(k, v) -> Endo $ T.replace ("${" <> k <> "}") v) vs

newtype RawConfig = RawConfig
  { unwrap :: MonoidalMap String RawExecutable
  }
  deriving newtype (Semigroup, Monoid, DecodeTOML)

data RawExecutable = RawExecutable
  { vars :: MonoidalMap Text (Last Text)
  , binary :: Maybe (Last Text)
  , install :: Maybe (Last Text)
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via (GenericSemigroupMonoid RawExecutable)

instance DecodeTOML RawExecutable where
  tomlDecoder =
    RawExecutable
      <$> getFieldOr mempty "vars"
      <*> getFieldOpt "binary"
      <*> getFieldOpt "install"

loadRawConfig :: FilePath -> IO RawConfig
loadRawConfig path = do
  exists <- doesFileExist path
  if exists
    then do
      result <- TOML.decodeFile path
      either throwIO pure result
    else pure mempty
