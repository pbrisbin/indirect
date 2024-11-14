module Indirect.Config
  ( Config (..)
  , Executable (..)
  , load
  ) where

import Indirect.Prelude

import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Map.Monoidal.Strict qualified as MonoidalMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup.Generic
import Path (parseAbsFile)
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.FilePath ((<.>), (</>))

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
resolveExecutable re = do
  binary <- parseAbsFile $ unpack $ interpolate vars $ getLast re.binary
  let install = unpack . interpolate vars . getLast <$> re.install
  pure Executable {binary, install}
 where
  vars :: [(Text, Text)]
  vars = map (second getLast) $ MonoidalMap.toList re.vars

interpolate :: [(Text, Text)] -> Text -> Text
interpolate = undefined

newtype RawConfig = RawConfig
  { unwrap :: MonoidalMap String RawExecutable
  }
  deriving newtype (Semigroup)

data RawExecutable = RawExecutable
  { vars :: MonoidalMap Text (Last Text)
  , binary :: Last Text
  , install :: Maybe (Last Text)
  }
  deriving stock (Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid RawExecutable)

loadRawConfig :: FilePath -> IO RawConfig
loadRawConfig = undefined
