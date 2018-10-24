module Type.NIL (NIL(..)) where

data NIL = NILWord  String
         | NILHash  String
         | NILInt   Int
         | NILFloat Float
         | NILList  [NIL]
         deriving (Read, Show)
