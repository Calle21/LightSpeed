module Type.Fixity (Fixity(..), win)

data Fixity = Infixl Int | Infixr Int | Prefix | Postfix
            deriving (Read, Show)

win :: (Fixity, Fixity) -> Either () ()
win (Postfix, _) = Left ()
win (_, Postfix) = Right ()
win (Prefix, _)  = Left ()
win (_, Prefix)  = Right ()
win (left,right) = let s0 = getStrength left
                       s1 = getStrength right
                   in if s0 > s1 then Left ()
                      else if s1 > s0 then Right ()
                           else case (left,right) of
                                  (Infixr _, Infixr _) -> Right ()
                                  _                    -> Left ()
  where
  getStrength :: Fixity -> Int
  getStrength (Infixl s) = s
  getStrength (Infixr s) = s
