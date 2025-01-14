{-# LANGUAGE ApplicativeDo #-}

module Indirect.Options
  ( Options (..)
  , Command (..)
  , ListOptions (..)
  , SetupOptions (..)
  , CleanOptions (..)
  , parseOptions
  ) where

import Indirect.Prelude

import Options.Applicative

newtype Options = Options
  { command :: Command
  }

data Command
  = List ListOptions
  | Setup SetupOptions
  | Clean CleanOptions

newtype ListOptions = ListOptions
  { only :: [String]
  }

data SetupOptions = SetupOptions
  { force :: Bool
  , install :: Bool
  , only :: [String]
  }

newtype CleanOptions = CleanOptions
  { only :: [String]
  }

parseOptions :: IO Options
parseOptions = execParser $ withInfo "Manage indirectly invokable executables" optionsParser

optionsParser :: Parser Options
optionsParser = Options <$> commandParser

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
      , command "clean"
          $ withInfo "Remove links and installed executables"
          $ Clean
          <$> cleanOptionsParser
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

cleanOptionsParser :: Parser CleanOptions
cleanOptionsParser = do
  only <-
    many
      $ argument str
      $ mconcat
        [ help "Limit to the given executable(s)"
        , metavar "NAME"
        ]

  pure CleanOptions {only}

withInfo :: String -> Parser a -> ParserInfo a
withInfo d p = info (p <**> helper) $ fullDesc <> progDesc d
