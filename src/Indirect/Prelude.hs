module Indirect.Prelude
  ( module X
  ) where

import Control.Monad as X (guard)
import Data.Foldable as X (for_)
import Data.Semigroup as X (Last (..))
import Data.Text as X (Text, pack, unpack)
import Data.Traversable as X (for)
import GHC.Generics as X (Generic)
import Path as X (Abs, Dir, File, Path, Rel, toFilePath)
import Prelude as X
