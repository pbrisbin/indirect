-- |
--
-- Module      : Indirect.Logging
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Indirect.Logging
  ( logInfo
  , highlightLinkName
  , highlightLinkTarget
  , highlightFile
  , module Data.Text.Escaped
  ) where

import Indirect.Prelude

import Data.String (fromString)
import Data.Text.Escaped
import Data.Text.IO qualified as T
import Path (filename, parent)
import System.IO (stderr)

logInfo :: Escaped -> IO ()
logInfo x = do
  r <- terminalRenderer
  T.hPutStrLn stderr $ r $ "[" <> blue "indirect" <> "] " <> x

highlightLinkName :: Path b File -> Escaped
highlightLinkName = highlightFile green

highlightLinkTarget :: Path b File -> Escaped
highlightLinkTarget = highlightFile cyan

highlightFile :: (Escaped -> Escaped) -> Path b File -> Escaped
highlightFile c path = magenta (toEscaped p) <> c (toEscaped f)
 where
  p = parent path
  f = filename path

toEscaped :: Path b t -> Escaped
toEscaped = fromString . toFilePath
