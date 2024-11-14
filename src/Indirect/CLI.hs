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

import Data.List.NonEmpty (some1)
import Data.Map.Strict qualified as Map
import Indirect.Config (Config (..), Executable (..))
import Options.Applicative
import Path (parseAbsDir, parseAbsFile, parseRelFile, (</>))
import Path.IO (createFileLink, doesFileExist, removeFile)
import System.Environment (getExecutablePath)
import System.Process.Typed (proc, runProcess_)

run :: Config -> IO ()
run config = do
  Setup options <- parseCommand

  for_ (Map.toList $ config.unwrap) $ \(name, exe) -> do
    when (maybe True (name `elem`) options.only) $ do
      putStrLn $ "Setting up " <> name

      for_ exe.install $ \install -> do
        when options.install $ do
          putStrLn $ "  => Installing " <> toFilePath exe.binary
          runProcess_ $ proc "sh" ["-c", install]

      for_ options.links $ \dir -> do
        self <- parseAbsFile =<< getExecutablePath
        link <-
          (</>)
            <$> parseAbsDir dir
            <*> parseRelFile name

        exists <- doesFileExist link

        when (exists && options.force) $ do
          putStrLn $ "  => Removing existing link " <> toFilePath link
          removeFile link

        when (not exists || options.force) $ do
          putStrLn $ "  => Linking " <> toFilePath link <> " to indirect executable"
          createFileLink self link

newtype Command = Setup Options

data Options = Options
  { only :: Maybe (NonEmpty String)
  , install :: Bool
  , links :: Maybe FilePath
  , force :: Bool
  }

parseCommand :: IO Command
parseCommand = execParser $ withInfo "" commandParser

commandParser :: Parser Command
commandParser =
  subparser
    $ command "setup"
    $ withInfo "Install and link defined executables"
    $ Setup
    <$> optionsParser

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional
      ( some1
          ( option
              str
              ( mconcat
                  [ long "only"
                  , help "Setup only the given executables"
                  , metavar "NAME"
                  ]
              )
          )
      )
    <*> ( not
            <$> switch
              ( mconcat
                  [ long "no-install"
                  , help "Don't run executable install stanzas"
                  ]
              )
        )
    <*> optional
      ( option
          str
          ( mconcat
              [ long "links"
              , help "Create symbolic links to the indirect executable as DIRECTORY/NAME"
              , metavar "DIRECTORY"
              ]
          )
      )
    <*> switch
      ( mconcat
          [ long "force"
          , help "Create symbolic links even if something exists there"
          ]
      )

withInfo :: String -> Parser a -> ParserInfo a
withInfo x p = info (p <**> helper) $ fullDesc <> progDesc x
