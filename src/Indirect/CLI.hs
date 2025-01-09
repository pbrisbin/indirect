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
import Data.String (fromString)
import Data.Text.Escaped
import Data.Text.IO qualified as T
import Indirect.Config (Config (..), Executable (..))
import Indirect.Executable
  ( doesExecutableExist
  , getTargetsDir
  , installExecutable
  )
import Indirect.Logging
import Indirect.Options
import Path (filename, parent, parseRelFile, (</>))
import Path.IO (createFileLink, doesFileExist, removeFile, withCurrentDir)

run :: Config -> IO ()
run config = do
  options <- parseOptions
  renderer <- terminalRenderer

  let
    bin = parent options.self
    indirect = filename options.self

    forExes only f =
      for_ (Map.toList $ config.unwrap) $ \(name, exe) -> do
        when (maybe True (name `elem`) $ nonEmpty only)
          $ withCurrentDir bin
          $ f name exe

  case options.command of
    List loptions -> do
      forExes loptions.only $ \name exe -> do
        link <- parseRelFile name
        exists <- doesFileExist link
        exeExists <- doesExecutableExist exe
        targets <- getTargetsDir
        let target = targets </> exe.binary

        T.putStrLn
          $ renderer
          $ green (fromString name)
          <> (if exists then " => " <> cyan "indirect" else "")
          <> (if exeExists then " => " <> highlightLinkTarget target else "")
    Setup soptions -> do
      forExes soptions.only $ \name exe -> do
        link <- parseRelFile name
        exists <- doesFileExist link

        let linkBinary = do
              logInfo
                $ "Linking "
                <> green (fromString name)
                <> " => "
                <> highlightLinkTarget options.self
              when exists $ removeFile link
              createFileLink indirect link

        case (exists, soptions.force, link /= indirect) of
          (_, _, False) -> pure () -- skip due to invalid link
          (True, False, _) -> pure () -- exists, skip due to no --force
          (_, True, True) -> linkBinary -- forcing
          (False, _, True) -> linkBinary -- missing
        exeExists <- doesExecutableExist exe

        case (exeExists, soptions.force, soptions.install) of
          (_, _, False) -> pure () -- skip due to --no-install
          (True, False, _) -> pure () -- exists, skip to to no --force
          (_, True, True) -> installExecutable name exe -- forcing
          (False, _, True) -> installExecutable name exe -- missing
    Clean coptions -> do
      forExes coptions.only $ \name exe -> do
        link <- parseRelFile name
        exists <- doesFileExist link
        exeExists <- doesExecutableExist exe

        when exists $ do
          logInfo $ "Removing " <> green (fromString name)
          removeFile link

        when exeExists $ do
          logInfo $ "Removing " <> green (fromString $ toFilePath exe.binary)
          removeFile exe.binary
