module Indirect.Executable
  ( findExecutable
  ) where

import Indirect.Prelude

import Control.Monad (liftM2)
import Data.Map.Strict qualified as Map
import Indirect.Config (Config (..), Executable (..))
import Path.IO (doesFileExist, executable, getPermissions)
import System.Process.Typed (proc, runProcess_)

findExecutable :: Config -> String -> IO (Maybe (Path Abs File))
findExecutable config pgname = do
  for (Map.lookup pgname config.unwrap) $ \exe -> do
    exists <- doesExecutableFileExist exe.binary

    let mInstall = do
          guard $ not exists
          exe.install

    for_ mInstall $ \install -> runProcess_ (proc "sh" ["-c", install])
    pure exe.binary

doesExecutableFileExist :: Path Abs File -> IO Bool
doesExecutableFileExist path =
  liftM2 (&&) (doesFileExist path) (executable <$> getPermissions path)
