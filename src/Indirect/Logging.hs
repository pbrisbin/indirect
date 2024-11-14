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
  ) where

import Indirect.Prelude

import System.IO (hPutStrLn, stderr)

logInfo :: String -> IO ()
logInfo x = hPutStrLn stderr $ "[indirect] " <> x
