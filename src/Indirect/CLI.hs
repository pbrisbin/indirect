{-# LANGUAGE ApplicativeDo #-}

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

import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Indirect.Config (Config (..), Executable (..))
import Indirect.Executable (installExecutable)
import Indirect.Logging
import Options.Applicative
import Path (parent, parseAbsDir, parseAbsFile, parseRelFile, (</>))
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

data Options = Options
  { links :: Path Abs Dir
  , command :: Command
  }

data Command = List | Setup SetupOptions

data SetupOptions = SetupOptions
  { force :: Bool
  , install :: Bool
  , only :: [String]
  }

parseOptions :: Path Abs File -> Config -> IO Options
parseOptions self config =
  execParser
    $ withInfo "" footer'
    $ optionsParser self footer'
 where
  footer'
    | null names = "Warning: no executables configured"
    | otherwise = "Configured executables: " <> intercalate ", " names

  names = Map.keys config.unwrap

optionsParser :: Path Abs File -> String -> Parser Options
optionsParser self footer' =
  Options
    <$> option
      (eitherReader $ first show . parseAbsDir)
      ( mconcat
          [ long "links"
          , help "Create symbolic links from DIRECTORY/NAME to indirect"
          , metavar "DIRECTORY"
          , showDefault
          , value (parent self)
          ]
      )
    <*> commandParser footer'

commandParser :: String -> Parser Command
commandParser footer' =
  subparser
    $ mconcat
      [ command "setup"
          $ withInfo "Link and install configured executables" footer'
          $ Setup
          <$> setupOptionsParser
      , command "ls" $ withInfo "Show configured executables" footer' $ pure List
      ]

setupOptionsParser :: Parser SetupOptions
setupOptionsParser = do
  force <-
    switch
      $ mconcat
        [ long "force"
        , help "Link and install even if something exists already"
        ]

  install <-
    fmap not
      $ switch
      $ mconcat
        [ long "no-install"
        , help "Don't pre-install executables"
        ]

  only <-
    many
      $ argument str
      $ mconcat
        [ help "Limit setup to the given executables"
        , metavar "NAME"
        ]

  pure SetupOptions {force, install, only}

withInfo :: String -> String -> Parser a -> ParserInfo a
withInfo d f p = info (p <**> helper) $ fullDesc <> progDesc d <> footer f
