{-# LANGUAGE ApplicativeDo #-}

module Indirect.Options
  ( Options (..)
  , Command (..)
  , ListOptions (..)
  , SetupOptions (..)
  , parseOptions
  ) where

import Indirect.Prelude

import Options.Applicative
import Path (parseAbsFile)
import System.Environment (getExecutablePath)

data Options = Options
  { self :: Path Abs File
  , command :: Command
  }

data Command = List ListOptions | Setup SetupOptions

newtype ListOptions = ListOptions
  { only :: [String]
  }

data SetupOptions = SetupOptions
  { force :: Bool
  , install :: Bool
  , only :: [String]
  }

parseOptions :: IO Options
parseOptions = do
  exe <- parseAbsFile =<< getExecutablePath
  execParser
    $ withInfo "Manage indirectly invokable executables"
    $ optionsParser exe

optionsParser :: Path Abs File -> Parser Options
optionsParser exe =
  Options
    <$> option
      (eitherReader $ first show . parseAbsFile)
      ( mconcat
          [ long "self"
          , help "Path to indirect executable to link to"
          , metavar "FILE"
          , value exe
          , internal
          ]
      )
    <*> commandParser

commandParser :: Parser Command
commandParser =
  subparser
    $ mconcat
      [ command "ls"
          $ withInfo "Show configured executables"
          $ List
          <$> listOptionsParser
      , command "setup"
          $ withInfo "Link executables and install targets"
          $ Setup
          <$> setupOptionsParser
      ]

listOptionsParser :: Parser ListOptions
listOptionsParser = do
  only <-
    many
      $ argument str
      $ mconcat
        [ help "Limit to the given executable(s)"
        , metavar "NAME"
        ]

  pure ListOptions {only}

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
        , help "Link executables, but don't install targets"
        ]

  only <-
    many
      $ argument str
      $ mconcat
        [ help "Limit to the given executable(s)"
        , metavar "NAME"
        ]

  pure SetupOptions {force, install, only}

withInfo :: String -> Parser a -> ParserInfo a
withInfo d p = info (p <**> helper) $ fullDesc <> progDesc d
