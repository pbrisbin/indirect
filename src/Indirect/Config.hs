module Indirect.Config
  ( Config (..)
  , Executable (..)
  , load
  ) where

import Indirect.Prelude

import Control.Exception (Exception, throwIO)
import Data.Bitraversable (bimapM)
import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Map.Monoidal.Strict qualified as MonoidalMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup.Generic
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

  either throwIO pure $ resolveConfig raw

newtype ConfigError = ConfigError
  { unwrap :: Text
  }
  deriving stock (Show)
  deriving anyclass (Exception)

resolveConfig :: RawConfig -> Either ConfigError Config
resolveConfig =
  fmap (Config . Map.fromList)
    . traverse (secondM resolveExecutable)
    . MonoidalMap.toList
    . (.unwrap)

resolveExecutable :: RawExecutable -> Either ConfigError Executable
resolveExecutable = undefined

newtype RawConfig = RawConfig
  { unwrap :: MonoidalMap String RawExecutable
  }
  deriving newtype (Semigroup)

data RawExecutable = RawExecutable
  { vars :: MonoidalMap Text (Last String)
  , binary :: Last String
  , install :: Maybe (Last String)
  }
  deriving stock (Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid RawExecutable)

loadRawConfig :: FilePath -> IO RawConfig
loadRawConfig = undefined

secondM :: Applicative f => (b -> f c) -> (a, b) -> f (a, c)
secondM = bimapM pure
