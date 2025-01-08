{-# LANGUAGE ApplicativeDo #-}

module Indirect.Options
  ( Options (..)
  , Command (..)
  , SetupOptions (..)
  , parseOptions
  ) where

import Indirect.Prelude

import Options.Applicative
import Path (parent, parseAbsDir)

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

parseOptions :: Path Abs File -> IO Options
parseOptions self =
  execParser
    $ withInfo "Manage indirectly invokable executables"
    $ optionsParser self

optionsParser :: Path Abs File -> Parser Options
optionsParser self =
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
    <*> commandParser

commandParser :: Parser Command
commandParser =
  subparser
    $ mconcat
      [ command "ls" $ withInfo "Show configured executables" $ pure List
      , command "setup"
          $ withInfo "Link executables and install targets"
          $ Setup
          <$> setupOptionsParser
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
        , help "Link executables, but don't install targets"
        ]

  only <-
    many
      $ argument str
      $ mconcat
        [ help "Limit setup to the given executables"
        , metavar "NAME"
        ]

  pure SetupOptions {force, install, only}

withInfo :: String -> Parser a -> ParserInfo a
withInfo d p = info (p <**> helper) $ fullDesc <> progDesc d
