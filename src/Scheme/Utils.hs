module Scheme.Utils (updateVector) where

import           Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

import           Scheme.Types


updateVector :: V.Vector LispValue -> Int -> LispValue -> V.Vector LispValue
updateVector xs n x = runST $ do
  mvector <- V.unsafeThaw xs
  M.write mvector n x
  V.unsafeFreeze mvector
