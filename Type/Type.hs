module TypesType (Type) where

type Type = (String, String, [String])

gettname, gettvars :: Type -> String
gettname (Type (n, _, _)) = n
gettvars (Type (_, v, _)) = v

gettdesc :: Type -> [String]
gettdesc (Type (_, _, d)) = d

instance Ord Type where
  compare :: Type -> Type -> Ordering
  compare (Type (n0, _, _)) (Type (n1, _, _)) = compare n0 n1

checkType :: Type -> Maybe Type
checkType xs = undefined

getTupleElements :: Type -> [Type]
getTupleElements t = if tuple t then split "," $ init $ tail t
                     else error "Called on single"

mkFNtype :: Type -> Type -> Type
mkFNtype ret param = ret ++ ["<-"] ++ param

safeGetTupleElements :: Type -> [Type]
safeGetTupleElements t = if single t then t else getTupleElements t

single :: Type -> Bool
single = not . tuple

tuple :: Type -> Bool
tuple (x:xs) = x == "("

typeCompat :: Setup -> Type -> Type -> Bool
typeCompat t0 t1 = let ts = [t0, t1]
                   in if all single ts then singleCompat ts
                      else if all tuple ts
                           then let [elts0,elts1] = getTupleElements `map` ts
                                in length elts0 == length elts1 && all (uncurry $ typeCompat setup) (elts0 `zip` elts1)
                           else False
  where
  singleCompat :: [Type] -> Bool
  singleCompat ts@[t0,t1] = if any vartype ts then True
                            else t0 == t1

typeConcat :: [Type] -> Type
typeConcat xs = "(" : intercalate [","] (safeGetTupleElements `map` xs) ++ [")"]

vartype :: Type -> Bool
vartype [[c]] = isUpper c
vartype _   = False

data TypeCheck = TypeCheck
