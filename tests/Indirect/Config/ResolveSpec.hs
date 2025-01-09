{-# LANGUAGE QuasiQuotes #-}

module Indirect.Config.ResolveSpec
  ( spec
  ) where

import Indirect.Prelude

import Indirect.Config
import Indirect.Config.Raw
import Indirect.Config.Resolve
import Indirect.Executable
import Path (relfile, (</>))
import Path.IO (withSystemTempFile)
import System.IO (hClose, hPutStrLn)
import Test.Hspec

spec :: Spec
spec = do
  describe "resolveConfig" $ do
    it "applies defaults and interpolations" $ do
      targets <- getTargetsDir
      config <-
        loadConfigLines
          [ "[defaults]"
          , "binary = \"${targets}/${name}-${version}\""
          , ""
          , "[foo]"
          , "vars.version = \"0.1.0.0\""
          , ""
          , "[bar]"
          , "vars.version = \"0.2.1.0\""
          ]

      foo <- findExecutable config "foo"
      foo `shouldBe` Just (targets </> [relfile|foo-0.1.0.0|])

      bar <- findExecutable config "bar"
      bar `shouldBe` Just (targets </> [relfile|bar-0.2.1.0|])

loadConfigLines :: [String] -> IO Config
loadConfigLines xs =
  withSystemTempFile "indirect.toml" $ \f h -> do
    hPutStrLn h $ unlines xs
    hClose h
    resolveConfig =<< loadRawConfig f
