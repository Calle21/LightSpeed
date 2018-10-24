module Type.Prim (Prim(..)) where

import Data.Word(Word)

data Prim = Signed Int | Unsigned Word deriving (Eq, Read, Show)

instance Ord Prim where
  compare :: Prim -> Prim -> Ordering
  compare (Signed i0)   (Signed i1)   = compare i0 i1
  compare (Unsigned w0) (Unsigned w1) = compare w0 w1
  compare (Signed i)    (Unsigned w)  = compare i (fromIntegral w)
  compare (Unsigned w)  (Signed i)    = compare (fromIntegral w) i
