-- |
--
-- Module      : Indirect.Prelude
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Indirect.Prelude
  ( module X
  , secondM
  , note
  ) where

import Control.Monad as X (guard, unless, void, when)
import Data.Bifunctor as X (bimap, first, second)
import Data.Bitraversable as X (bimapM)
import Data.Foldable as X (for_)
import Data.List.NonEmpty as X (NonEmpty (..), nonEmpty)
import Data.Maybe as X (catMaybes, fromMaybe, mapMaybe)
import Data.Semigroup as X (Last (..))
import Data.String as X (IsString (..))
import Data.Text as X (Text, pack, unpack)
import Data.Traversable as X (for)
import GHC.Generics as X (Generic)
import Indirect.Orphans ()
import Path as X (Abs, Dir, File, Path, Rel, toFilePath)
import Prelude as X

secondM :: Applicative f => (b -> f c) -> (a, b) -> f (a, c)
secondM = bimapM pure

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right
