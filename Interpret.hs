module Interpret (eval) where

data NovaData = NovaData Desc [NovaData]

data Desc = Int
          | Table Desc
          | Tuple [Desc]
          | Type String

eval :: [(Token,String,Token)]
     -> [Library]
     -> Maybe Token
     -> Maybe Int
     -> Token
     -> Token
eval locals libs et argLength exp =
  case exp of
    Access s st      -> 
    Array _ _        -> exp
    As t tk          -> let tk' = eval locals libs et (Just 0) tk
                        in if typeCompat t (typeOf tk')
                           then Type t tk'
                           else error' "Type incompatibility"
    From lib tk      -> let lib' = getLib lib libs
                        in getMatches (nameString tk)
    Funcall fn args  -> let fn'   = eval locals libs et      (getArgLength args) fn
                            args' = eval locals libs Nothing (Just 0)            fn
                        in apply fn' args
    Function _ _ _   -> exp
    If bool cl0 cl1  ->
    Int _            -> exp
    Let binds body   ->
    Modify tk mods   -> 
    Name s           -> getMatches s et argLength libs
    Pure tk          ->
    Seq tks          ->
    Struct _ _       -> exp
    Switch tk cls    -> let tk' = eval locals libs et argLength tk
                        in 
    TCase tk cls def ->
    The tp tk        ->
    Tuple _          -> exp
    Type _ _         -> exp
  where
  apply :: Token -> Token -> Token
  apply (Function rt params body) args

getMatches :: String -> Maybe Token -> Int -> [(Token,String,Token)] -> [Token]
getMatches name rt argLength ((a,b,c):xs)
 | name == b && typeMatch rt a = c : getMatches name rt xs
 | otherwise                   = getMatches name rt xs

typeMatch :: Maybe Token -> Token -> Bool
typeMatch Nothing   _  = True
typeMatch (Just et) tp = et == getReturnType tp

getReturnType :: Token -> Token
getReturnType (FN rt _) = rt
getReturnType tp        = tp

getArgLength :: Token -> Int
getArgLength (Tuple args)  = Just $ length args
getArgLength (Funcall _ _) = Nothing

typeOf :: Token -> Token
typeOf (Access s tk) =
typeOf (Array dim tks) = AR (length dim)
                            (typeOf $ head tks)
typeOf (As s tk) =
typeOf (From lib tk) =
typeOf (Funcall fn args) =
typeOf (Function rt params _) = ...
typeOf (If bool cl0 cl1) =
typeOf (Int _) = IN
typeOf (Let binds body) =
typeOf (Modify tk mods) =
typeOf (Name s)         =
typeOf (Pure tk)        =
typeOf (Seq tks)        = typeOf (last tks)
typeOf (Struct s tks)   =
typeOf (Switch tk cls)  =
typeOf (TCase tk cls def) =
typeOf (The tp _)      = tp
typeOf (Tuple tks)      =
typeOf (Type s tk)      =

getLib :: String -> [Library] -> Library
getLib s (x:xs) | s == libNm x = x
                | otherwise    = getLib s xs
getLib s [] = error' ("No such library (" ++ s ++ ")"

nameString :: Token -> String
nameString (Name s) = s
nameString _        = error' "Couldn't get name string"

applyType :: Token -> [Token] -> Token
applyType (PR tv tp) tps
  | length tps /= length tv = error' "Apply type (arguments mismatch)"
  | null tv                 = tp
  | otherwise               = replace tp
  where
  replace :: Token -> Token
  replace (AllCap s)    = case s `elemIndex` tv of
                            Nothing -> error' ("No such type variable (" ++ s ++ ")")
                            Just n  -> tps ! n
  replace (AR n tk)     = AR n (replace tk)
  replace (FN rt pt)    = FN (replace rt) (replace pt)
  replace IN            = IN
  replace (LU s tp)     = LU s (replace tp)
  replace (ST s fields) = ST s (map replaceField fields)
    where
    replaceField :: (Token,String) -> (Token,String)
    replaceField (tk,s) = (replace tk,s)
  TU tks                = TU $ map replace tks
  TP s tk               = TP s (replace tk)
  UN tks                = UN $ map replace tks

 -- Error

error' :: String -> a
error' message = error ("Runtime error: " ++ message)
