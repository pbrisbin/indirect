module Indirect.CLI
  ( run
  ) where

import Indirect.Prelude

run :: IO ()
run = putStrLn "Called not symlinked as an executable"
