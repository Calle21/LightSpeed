module Type.PMonad (PMonad) where

import Type.Tok

type PMonad = [Tok] -> Maybe [Tok]
