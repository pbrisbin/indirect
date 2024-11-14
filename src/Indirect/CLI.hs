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
import Indirect.Config (Config (..))
import Indirect.Executable (installExecutable)
import Indirect.Logging
import Options.Applicative
import Path (parseAbsDir, parseAbsFile, parseRelFile, (</>))
import Path.IO (createFileLink, doesFileExist, removeFile)
import System.Environment (getExecutablePath)

run :: Config -> IO ()
run config = do
  Setup options <- parseCommand config

  for_ (Map.toList $ config.unwrap) $ \(name, exe) -> do
    when (maybe True (name `elem`) $ nonEmpty options.only) $ do
      when options.install $ installExecutable name exe

      for_ options.links $ \dir -> do
        self <- parseAbsFile =<< getExecutablePath
        link <-
          (</>)
            <$> parseAbsDir dir
            <*> parseRelFile name

        exists <- doesFileExist link

        when (exists && options.force) $ do
          logInfo $ "Removing existing link " <> toFilePath link
          removeFile link

        when (not exists || options.force) $ do
          logInfo $ "Linking " <> toFilePath link <> " to indirect executable"
          createFileLink self link

newtype Command = Setup Options

data Options = Options
  { links :: Maybe FilePath
  , force :: Bool
  , install :: Bool
  , only :: [String]
  }

parseCommand :: Config -> IO Command
parseCommand config =
  execParser
    $ withInfo "" footer'
    $ commandParser footer'
 where
  footer'
    | null names = "Warning: no executables configured"
    | otherwise = "Configured executables: " <> intercalate ", " names

  names = Map.keys config.unwrap

commandParser :: String -> Parser Command
commandParser footer' =
  subparser
    $ command "setup"
    $ withInfo "Link and install configured executables" footer'
    $ Setup
    <$> optionsParser

optionsParser :: Parser Options
optionsParser = do
  links <-
    optional
      $ option str
      $ mconcat
        [ long "links"
        , help "Create symbolic links from DIRECTORY/NAME to indirect"
        , metavar "DIRECTORY"
        ]

  force <-
    switch
      $ mconcat
        [ long "force"
        , help "Create symbolic links even if something exists there"
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

  pure Options {links, force, install, only}

withInfo :: String -> String -> Parser a -> ParserInfo a
withInfo d f p = info (p <**> helper) $ fullDesc <> progDesc d <> footer f
