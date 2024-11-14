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
resolveConfig =
  fmap (Config . Map.fromList)
    . traverse (secondM resolveExecutable)
    . MonoidalMap.toList
    . (.unwrap)

resolveExecutable :: RawExecutable -> IO Executable
resolveExecutable re =
  Executable
    <$> parseAbsFile (unpack binaryT)
    <*> pure install
 where
  vars :: [(Text, Text)]
  vars = map (second getLast) $ MonoidalMap.toList re.vars

  binaryT = interpolate vars $ getLast re.binary
  binaryV = ("binary", binaryT)
  install = unpack . interpolate (binaryV : vars) . getLast <$> re.install

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
  , binary :: Last Text
  , install :: Maybe (Last Text)
  }
  deriving stock (Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid RawExecutable)

instance DecodeTOML RawExecutable where
  tomlDecoder =
    RawExecutable
      <$> getField "vars"
      <*> getField "binary"
      <*> getFieldOpt "install"

loadRawConfig :: FilePath -> IO RawConfig
loadRawConfig path = do
  exists <- doesFileExist path
  if exists
    then do
      result <- TOML.decodeFile path
      either throwIO pure result
    else pure mempty
