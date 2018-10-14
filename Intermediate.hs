module Intermediate () where

import Data.Word(Word)

data Intermediate = Instr string
                  | Param Int
                  | Signed Int (Maybe Int)
                  | Unsigned Word (Maybe Int)
                  | Floating Double (Maybe (Int,Int))
                  | Sub Intermediate
