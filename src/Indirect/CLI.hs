-- |
--
-- Module      : Indirect.CLI
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Indirect.CLI
  ( run
  ) where

import Indirect.Prelude

import Data.Map.Strict qualified as Map
import Indirect.Config (Config (..), Executable (..))
import Indirect.Executable (installExecutable)
import Indirect.Logging
import Indirect.Options
import Path (parseAbsFile, parseRelFile, (</>))
import Path.IO (createFileLink, doesFileExist, removeFile)
import System.Environment (getExecutablePath)

run :: Config -> IO ()
run config = do
  self <- parseAbsFile =<< getExecutablePath
  options <- parseOptions self config

  case options.command of
    List -> do
      for_ (Map.toList $ config.unwrap) $ \(name, exe) -> do
        link <- (options.links </>) <$> parseRelFile name
        exists <- doesFileExist link
        putStrLn
          $ name
          <> " => "
          <> toFilePath exe.binary
          <> (if exists then "" else " (missing)")
    Setup soptions -> do
      for_ (Map.toList $ config.unwrap) $ \(name, exe) -> do
        when (maybe True (name `elem`) $ nonEmpty soptions.only) $ do
          when soptions.install $ do
            installExecutable soptions.force name exe

            link <- (options.links </>) <$> parseRelFile name
            exists <- doesFileExist link

            when (exists && soptions.force) $ do
              logInfo $ "Removing existing link " <> toFilePath link
              removeFile link

            when ((not exists || soptions.force) && self /= link) $ do
              logInfo $ "Linking " <> toFilePath link <> " to indirect executable"
              createFileLink self link
