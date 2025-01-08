{-# LANGUAGE ApplicativeDo #-}

module Indirect.Options
  ( Options (..)
  , Command (..)
  , SetupOptions (..)
  , parseOptions
  ) where

import Indirect.Prelude

import Options.Applicative
import Path (parent, parseAbsDir, parseAbsFile)
import System.Environment (getExecutablePath)

data Options = Options
  { links :: Path Abs Dir
  , command :: Command
  }

data Command = List | Setup SetupOptions

data SetupOptions = SetupOptions
  { self :: Path Abs File
  , force :: Bool
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
      (eitherReader $ first show . parseAbsDir)
      ( mconcat
          [ long "links"
          , help "Create symbolic links from DIRECTORY/NAME to indirect"
          , metavar "DIRECTORY"
          , showDefault
          , value (parent exe)
          ]
      )
    <*> commandParser exe

commandParser :: Path Abs File -> Parser Command
commandParser exe =
  subparser
    $ mconcat
      [ command "ls" $ withInfo "Show configured executables" $ pure List
      , command "setup"
          $ withInfo "Link executables and install targets"
          $ Setup
          <$> setupOptionsParser exe
      ]

setupOptionsParser :: Path Abs File -> Parser SetupOptions
setupOptionsParser exe = do
  self <-
    option
      (eitherReader $ first show . parseAbsFile)
      ( mconcat
          [ long "self"
          , help "Path to indirect executable to link to"
          , metavar "FILE"
          , showDefault
          , value exe
          ]
      )
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
        [ help "Limit setup to the given executables"
        , metavar "NAME"
        ]

  pure SetupOptions {self, force, install, only}

withInfo :: String -> Parser a -> ParserInfo a
withInfo d p = info (p <**> helper) $ fullDesc <> progDesc d
