module Types where

import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ubi
import Util(String')

 -- Binding

data Binding = Binding {locals  :: [Binding],
                        bindpat :: Pattern,
                        params  :: BindPat,
                        value   :: Code}
              deriving (Read, Show)

 -- BindPat

type BindPat = Tuple [BindPat]
             | Single (Maybe Type) String'
             | Callable [BindPat]

 -- Prim

data Prim = Signed Int | Unsigned Int deriving (Eq, Read, Show)

instance Ord Prim where
  compare (Signed i0)   (Signed i1)   = compare i0 i1
  compare (Unsigned w0) (Unsigned w1) = compare w0 w1
  compare (Signed i)    (Unsigned w)  = compare i (fromIntegral w)
  compare (Unsigned w)  (Signed i)    = compare (fromIntegral w) i

 -- Code

data Code = Array      Type   [Code]
          | Bind       BindPat Code
          | OpCall     String' Code
          | Catch      String'
          | From       String' String'
          | Let        Locals  Code
          | PatCall    Pattern
          | Primitive  Prim    Integer
          | The        Type    Code
          | Throw      String' Code
          | Tuple             [Code]
          deriving (Eq, Ord, Read, Show)

data Pattern = Identifier    String'
             | SomethingElse Code

 -- CompFN

type CompFN = FilePath -> CompFile -> CompFile

 -- File

data File = Folder   FilePath Folder
          | CompFile FilePath CompFile
          deriving (Read, Show)

countFiles :: Folder -> Int
countFiles (x:xs) = case x of
                      CompFile _ _  -> 1             + countFiles xs
                      Folder   _ fd -> countFiles fd + countFiles xs
countFiles []     = 0

mapFolder :: Folder -> CompFN -> Folder
mapFolder (x:xs) fn = case x of
                        CompFile p fl -> let file = fn p fl
                                         in CompFile p file : mapFolder xs fn
                        Folder   p fd -> Folder p (mapFolder fd fn) : mapFolder xs fn
mapFolder []     _  = []

mapFolderM :: Folder -> FolderM -> IO ()
mapFolderM (x:xs) act = case x of
                          CompFile p fl -> do act p fl
                          Folder   _ fd -> do mapFolderM fd act
                                              mapFolderM xs act
mapFolderM []     _   = return ()

 -- Folder

type Folder = [File]

 -- FolderM

type FolderM = FilePath -> CompFile -> IO ()

 -- CompFile

data CompFile = Undone   C.ByteString
              | Lexed    [Tok2]
              | Indented Indent
              | Parsed   Code
              | Compiled NIL
              deriving (Read, Show)

 -- Fixity

data Fixity = Infixl | Infixr | Prefix | Postfix

data Fixity = Infixl Int | Infixr Int | Prefix | Postfix
            deriving (Read, Show)

fixWin :: (Fixity, Fixity) -> Either () ()
fixWin (Postfix, _) = Left ()
fixWin (_, Postfix) = Right ()
fixWin (Prefix, _)  = Left ()
fixWin (_, Prefix)  = Right ()
fixWin (left,right) = let s0 = getStrength left
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

 -- Function

data Callable = Callable {callableOffsetInLibrary :: Either NIL Int, -- Inline or offset
                          callablePattern         :: [Pat],
                          callableIsPure          :: Bool}
{-
data Function = Function {fntype  :: FunctionType,
                          rettype :: [String],
                          pattern :: [Pattern],
                          body    :: Code,
                          setup   :: Setup}
              deriving (Read, Show)
-}
data Library = Library {libraryLocation        :: FilePath,
                        libraryParserSpecifics :: ParserSpec,
                        libraryCallables       :: [Callable]}

 -- Indent

data Line = Line Int [Tok1]
          | Fold [Line]
          deriving (Read, Show)

colOf  (Line _ xs) = tok1Col (head xs)
colOf  (Fold ys)   = colOf (head ys)
lineOf (Line ln _) = ln
lineOf (Fold ys)   = lineOf (head ys)

 -- Lib

type LChain = [(String, String)]

type LFix = Map String Fixity

data Lib = Lib {autotags :: [([String],String)],
                bindings :: [Binding],
                chains   :: [(String,String)],
                fixity   :: Map.Map String Fixity,
                tags     :: Map.Map String Type BindPat Code,
                types    :: Set.Set Type}
         deriving (Read, Show)

type LTag = [([String],String)]

type LTyp = Map [String] [String]

type LVal = Map Identifier [Binding]

 -- Local

data Local = Local {bind        :: Binding,
                    annotations :: [String]}
           | Locals [Binding]
           deriving (Read, Show)

 -- NIL

data NIL = NILWord  String
         | NILHash  String
         | NILInt   Int
         | NILFloat Float
         | NILList  [NIL]
         deriving (Read, Show)

 -- ParserSpec

data PSpec = PSpec {fixities :: Map String' Fixity
                  , chains   :: [(String',String')]}

 -- ParseTree

data ParseTree = Let     [(String,ParseTree)]
               | Make    [(String,ParseTree)]
               | Call    String [ParseTree]
               | Literal Literal

data Literal = Array     Type [ParseTree]
             | Primitive Primitive

data Primitive = Unsigned Word64
               | Signed   Int64
               | Float    Double
               | Ratio    Ratio

 -- Pat

data Pat = PatK String'
         | PatT Type
         deriving (Eq, Ord, Read, Show)

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

 -- Pattern

newtype Pattern = Pattern (Type, [Pat]) deriving (Eq, Ord, Read, Show)

patternCompat = undefined
 -- patternCompat :: Setup -> Pattern -> Pattern -> Bool
 -- patternCompat setup (ret0, pat0) (ret1, pat1) = typeCompat setup ret0 ret1 && every (uncurry patCompat) (pat0 `zip` pat1)

 -- PMonad

type PMonad = [Tok1] -> Maybe [Tok1]

 -- SpecialParse

type SpecialParse = (Setup, FilePath, [Indent]) -> (Setup, [Indent])

 -- Tok

data Tok = Keyword       String'
         | Opname        String'
         | Option        String'
         | Punctuation   Char
         | Reserved      String'
         | Special       String'
         | TokChar       Char
         | TokFloat      Float
         | TokInt        Int
         | TokString     String'
         | Type          String'
         | Typevar       String'
         deriving (Eq, Read, Show)

tokChar   (Punctuation c) = c
tokChar   (TokChar c)     = c
tokFloat  (TokFloat f)    = f
tokInt    (TokInt i)      = i
tokString (Keyword s')    = s'
tokString (Opname s')     = s'
tokString (Reserved s')   = s'
tokString (Special s')    = s'
tokString (TokString s')  = s'
tokString (Type s')       = s'
tokString (Typevar s')    = s'

 -- Tok1

type Tok1 = (Int, Tok)

tok1Col :: Tok1 -> Int
tok1Col = fst

tok1Tok :: Tok1 -> Tok
tok1Tok = snd

tok1Char :: Tok1 -> Char
tok1Char = tokChar . tok1Tok

tok1Float :: Tok1 -> Float
tok1Float = tokFloat . tok1Tok

tok1Int :: Tok1 -> Int
tok1Int = tokInt . tok1Tok

tok1String :: Tok1 -> String'
tok1String = tokString . tok1Tok

 -- Tok2

type Tok2 = (Int, Int, Tok)

tok2Line, tok2Col :: Tok2 -> Int -- depr
tok2Line (ln,_,_) = ln
tok2Col  (_,cl,_) = cl

lineTok2, colTok2 :: Tok2 -> Int
lineTok2 (ln,_,_) = ln
colTok2  (_,cl,_) = cl

tok2Tok :: Tok2 -> Tok -- depr
tok2Tok (_,_,tk) = tk

tokTok2 :: Tok2 -> Tok
tokTok2 (_,_,tk) = tk

 -- TokP

type TokP = Tok -> Bool

 -- Type

type Type = (String, String, [String])

typeCompat = undefined
typeConcat = undefined

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
