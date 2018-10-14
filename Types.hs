module Nova.Types where

import Data.Map.Strict (Map)
import Prelude hiding (getLine)

data Binding = Binding {locals  :: [Binding],
                        pattern :: Pattern,
                        params  :: BindPat
                        value   :: Code}
              deriving (Read, Show)

 {- instance Ord Binding where
  compare :: Binding -> Binding -> Ordering
  compare b0 b1 = compare (pattern b0) (pattern b1) -}


type BindPat = [String]


data Code = Array      [String] [Code]
          | Callop     String Code
          | Catch      String
          | Defspecial String Code
          | From       String String
          | Let        Environment Code
          | Name       String
          | Pattern    [Code]
          | Primitive  Prim Integer
          | The        [String] Code
          | Throw      String Code
          | Tuple      [Code]
          deriving (Eq, Ord, Read, Show)


data Fixity = Infixl Int | Infixr Int | Prefix | Postfix deriving (Read, Show)

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


data Function = Function (fntype  :: FunctionType,
                          rettype :: [String],
                          pattern :: [Pattern],
                          body    :: Code,
                          locals  :: Setup)
              deriving (Read, Show)


data FunctionType = Action | Pure deriving (Eq, Read, Show, Enum)


type Identifier = String

data Indent = Line Int [Tok]
            | Indent [Indent]
            deriving (Read, Show)

getLine, getColumn :: Indent -> Int
getLine   (Line ln _)    = ln
getLine   (Indent (y:_)) = getLine y
getColumn (Line _ (x:_)  = fst x
getColumn (Indent (y:_)) = getColumn y


type Lex = (Int, Int, Token)

getcl, getln :: Lex -> Int
getcl (c, _, _) = c
getln (_, l, _) = l

gettk :: Lex -> Token
gettk (_, _, t) = t


data Local = Local {bind        :: Binding,
                    annotations :: [String])
           | Locals [Binding]
           deriving (Read, Show)


type MChain = [(String, String)]

type MFix = Map String Fixity

data Module = Module {autotags :: [([String],String)],
                      bindings :: [Binding],
                      chains   :: [(String,String)],
                      fixity   :: Map String Fixity,
                      tags     :: Map String Type BindPat Code
                      types    :: Map [String] [String]}
              deriving (Read, Show)

type MTag = [([String],String)]

type MTyp = Map [String] [String]

type MVal = Map Identifier [Binding]


data Pat = PatK String
         | PatT Type
         deriving (Ord, Read, Show)

patCompat :: Setup -> Pat -> Pat -> Bool
patCompat _     (PatK s0) (PatK s1) = s0 == s1
patCompat _     (PatK _)  (PatT _)  = True
patCompat setup (PatT t0) (PatT t1) = typeCompat setup t0 t1
patCompat _     _         _         = False

getTypeAsTuple :: [Pat] -> Type
getTypeAsTuple pat = typeConcat $ filterIt pat
  where
  filterIt ((PatT t):xs) = t : filterIt xs
  filterIt ((PatK _):xs) = filterIt xs
  filterIt []            = []


newtype Pattern = Pattern (Type, [Pat]) deriving (Ord, Read, Show)

patternCompat :: Setup -> Pattern -> Pattern -> Bool
patternCompat setup (ret0, pat0) (ret1, pat1) = typeCompat setup ret0 ret1 && every (uncurry patCompat) (pat0 `zip` pat1)


type PMonad = [Tok] -> Maybe [Tok]


data Prim = Signed Int | Unsigned Word deriving (Eq, Read, Show)

instance Ord Prim where
  compare :: Prim -> Prim -> Ordering
  compare (Signed i0)   (Signed i1)   = compare i0 i1
  compare (Unsigned w0) (Unsigned w1) = compare w0 w1
  compare (Signed i)    (Unsigned w)  = compare i (fromIntegral w)
  compare (Unsigned w)  (Signed i)    = compare (fromIntegral w) i


type Setup = [(String,Module)]


type SpecialParse -> (Setup, FilePath, [Indent]) -> (Setup, [Indent])


type Tok = (Int, Token)

getc :: Tok -> Char
getc (_, t) = getC t

getf :: Tok -> Float
getf (_, t) = getF t

geti :: Tok -> Int
geti (_, t) = getI t

gets :: Tok -> String
gets (_, t) = getS t


data Token = AChar       Char
           | AFloat      Float
           | AInt        Int
           | AString     String
           | HashI       Int
           | HashBind    String
           | Keyword     String
           | Op          String
           | Punctuation Char
           | Reserved    String
           | Special     String
           | Tag         String
           | Type        String
           | Vartype     String
           deriving (Read, Show)

getC :: Token -> Char
getC (AChar c)       = c
getC (Punctuation c) = c

getF :: Token -> Float
getF (AFloat f) = f

getI :: Token -> Int
getI (AInt i)  = i
getI (HashI i) = i

getS :: Token -> String
getS (AString s)  = s
getS (HashBind s) = s
getS (Keyword s)  = s
getS (Op s    )   = s
getS (Reserved s) = s
getS (Special s)  = s
getS (Tag s)      = s
getS (Type s)     = s
getS (Vartype s)  = s


type TokP = Tok -> Bool


type Type = [String]

checkType -> Type -> Maybe Type
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
