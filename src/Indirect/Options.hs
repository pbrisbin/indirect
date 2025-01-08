{-# LANGUAGE ApplicativeDo #-}

module Indirect.Options
  ( Options (..)
  , Command (..)
  , SetupOptions (..)
  , parseOptions
  ) where

import Indirect.Prelude

import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Indirect.Config (Config (..))
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
