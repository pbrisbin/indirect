module Indirect.Prelude
  ( module X
  , secondM
  , note
  ) where

import Control.Monad as X (guard)
import Data.Bifunctor as X (first, second)
import Data.Bitraversable as X (bimapM)
import Data.Foldable as X (for_)
import Data.Semigroup as X (Last (..))
import Data.Text as X (Text, pack, unpack)
import Data.Traversable as X (for)
import GHC.Generics as X (Generic)
import Path as X (Abs, Dir, File, Path, Rel, toFilePath)
import Prelude as X

secondM :: Applicative f => (b -> f c) -> (a, b) -> f (a, c)
secondM = bimapM pure

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right
