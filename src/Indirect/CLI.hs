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
        linkExists <- doesFileExist link
        execExists <- doesExecutableExist exe
        targets <- getTargetsDir
        let target = targets </> exe.binary

        T.putStrLn
          $ renderer
          $ green (fromString name)
          <> " -> "
          <> highlightLinkTarget target
          <> (if linkExists then "" else red " (missing link)")
          <> (if execExists then "" else yellow " (needs install)")
    Setup soptions -> do
      forExes soptions.only $ \name exe -> do
        link <- parseRelFile name
        linkExists <- doesFileExist link
        execExists <- doesExecutableExist exe

        let linkBinary = do
              logInfo
                $ "Linking "
                <> green (fromString name)
                <> " => ./"
                <> cyan (fromString $ toFilePath link)
              when linkExists $ removeFile link
              createFileLink indirect link

        case (linkExists, soptions.force, link /= indirect) of
          (_, _, False) -> pure () -- skip due to invalid link
          (True, False, _) -> pure () -- linkExists, skip due to no --force
          (_, True, True) -> linkBinary -- forcing
          (False, _, True) -> linkBinary -- missing
        case (execExists, soptions.force, soptions.install) of
          (_, _, False) -> pure () -- skip due to --no-install
          (True, False, _) -> pure () -- linkExists, skip to to no --force
          (_, True, True) -> installExecutable name exe -- forcing
          (False, _, True) -> installExecutable name exe -- missing
    Clean coptions -> do
      forExes coptions.only $ \name exe -> do
        link <- parseRelFile name
        linkExists <- doesFileExist link
        execExists <- doesExecutableExist exe

        when linkExists $ do
          logInfo $ "Removing " <> green (fromString name)
          removeFile link

        when execExists $ do
          logInfo $ "Removing " <> green (fromString $ toFilePath exe.binary)
          removeFile exe.binary
