module Parse (parse)

import Data.Maybe ((>>=),isJust)
import Interpret (Desc(..))
import Lex (Lex(..))

data Top = TopClass [Class] String [String] [Field]
         | TopBind [String] BindPat Expr
         | TopEnum String [String] [String]
         | TopStruct [Class] String [String] [Field]
         | TopSynonym String [String] Type
         | TopType String [String] Type
         | TopUnion String [String] [Type]
         | TopUse [String]
         deriving (Show)

type TopReader = Int -> FilePath -> [Lex] -> (Top,Int,Lex)

data Expr = ExprArray [Expr]
          | ExprArrayFromTo Expr Expr
          | ExprArrayFromThenTo Expr Expr Expr
          | ExprArraySeqBuild ...
          | ExprAs String Expr
          | ExprChar Char
          | ExprDefault
          | ExprDoUntil Expr Expr
          | ExprDoWhile Expr Expr
          | ExprEach [(BindPat,Expr)] Expr
          | ExprFor String Int Int Expr
          | ExprFrom String Expr
          | ExprFromThenTo Expr Expr Expr
          | ExprFromTo Expr Expr
          | ExprFuncall Expr Expr
          | ExprHash Int
          | ExprIf Expr Expr Expr
          | ExprInfixCalls [String] [Expr]
          | ExprInt Int64
          | ExprLambda Type BindPat Expr
          | ExprLet [(BindPat,Expr)] Expr
          | ExprList [Expr]
          | ExprLoop String Type BindPat Expr
          | ExprModify Expr [(Expr,Expr)]
          | ExprPure Expr
          | ExprSeq [Expr]
          | ExprSeqBuild ...
          | ExprStatement [Expr]
          | ExprString String
          | ExprSwitch Expr [(Expr,Expr)]
          | ExprTCase Expr [(Type,Expr)]
          | ExprThe Type Expr
          | ExprTuple [Expr]
          | ExprTypeSeqBuild ...
          | ExprUntil Expr Expr
          | ExprWhile Expr Expr
          deriving (Show)

type ExprReader = Int -> FilePath -> [Lex] -> (Expr,Int,[Lex])

readExpr :: ReadMode -> Either Int Int -> ExprReader
readExpr mode indent ln filename xs = 
  let c = getCol xs
  in if goodIndent c
     then let (exps,ln0,xs0) = readStatements mode c ln filename xs
          in (mkSeq exps,ln0,xs0
     else error' ln filename "Bad indentation"
  where
  getCol :: [Lex] -> Int
  getCol (SetCol c:_) = c
  getCol _            = error ln filename "Expected expression"
  goodIndent :: Int -> Bool
  goodIndent c = case indent of
                   Left c'  -> c > c'
                   Right c' -> c == c'
  mkSeq :: [Expr] -> Expr
  mkSeq [x] = x
  mkSeq xs  = ExprSeq xs

readStatements :: ReadMode -> Int -> Int -> FilePath -> [Lex] -> ([Expr],Int,[Lex])
raedStatements mode indent ln filename xs =
  let (state,ln0,xs0) = readStatement mode ln filename xs
  in case get xs0 of
       (_,SetLine ln1,xs1)  -> if mode == Norm && getCol xs1 == indent
                               then let (rest,ln2,xs2) = readStatements mode indent ln1 filename xs1
                                    in (state : rest,ln2,xs2)
                               else ([state],ln0,xs0)
       (_,Keyword ">>",xs1) -> let (rest,ln2,xs2) = readStatements mode indent ln0 filename xs1
                               in (state : rest,ln2,xs2)
       _                    -> ([state],ln0,xs0)

readStatement :: ReadMode -> ExprReader
readStatement mode ln filename xs = let (subs,ln0,xs0) = readSubs mode ln filename xs
                                    in if null subs then error "Expected sub-expressions"
                                       else (ExprStatement subs,ln0,xs0)

readSubs :: ReadMode -> Int -> FilePath -> [Lex] -> ([Expr],Int,[Lex])
readSubs mode ln filename xs = let (s0,ln0,xs0) = readSub mode ln filename xs
                               in case s0 of
                                    Nothing  -> ([],ln0,xs0)
                                    Just s0' -> let (rest,ln1,xs1) = readSubs mode ln0 filename xs0
                                                in (s0 : rest,ln1,xs1)

readSub :: ReadMode -> Int -> FilePath -> [Lex] -> (Maybe Expr,Int,[Lex])
readSub mode ln filename xs = let (a,ln0,xs0) = readAtom ln filename xs
                              in case a of
                                   Nothing     -> (a,ln0,xs0)
                                   Just (Op _) -> (a,ln0,xs0)
                                   Just a'     -> let (a0,ln1,xs1) = readAtom ln0 filename xs0
                                                  in case a0 of
                                                       Nothing     -> (a,ln1,xs1)
                                                       Just (Op _) -> (a,ln0,xs0)
                                                       Just a0'    -> (ExprFuncall a' a0',ln1,xs1)

data Type = TypeAny
          | TypeArray Type
          | TypeDerive
          | TypeFunction Type Type
          | TypeInt
          | TypeName String [Type]
          | TypeProduce [String] Type
          | TypeTuple [Type]
          | TypeType String
          | TypeUnion [Type]
          | TypeVar String
          deriving (Show)

data BindPat = BindSimple Type String
             | BindLambda Type BindPat
             | BindProcedure Type String BindPat
             | BindTuple [BindPat] (Maybe String)
             | BindArray [BindPat] (Maybe String)
             | BindList [BindPat] (Maybe String)
             | BindCons
             | BindType Type
             | BindName String
             | BindOp String
             | BindIgnore
             | BindVoid
             deriving (Show)

data DelimiterExpr = DelimiterExpr Expr
                   | DelimiterType Type
                   | DelimiterParam Param
                   | DelimiterModifier Expr Expr
                   | DelimiterField Field
                   | DelimiterAll String
                   | DelimiterClass String [Type]
                   | DelimiterBindpats [Bindpat]

delimiterExtractExpr :: DelimiterExpr -> Expr
delimiterExtractExpr (DelimiterExpr exp) = exp

delimiterExtractType :: DelimiterExpr -> Type
delimiterExtractType (DelimiterType t) = t

delimiterExtractParam 

delimiterExtractAll :: DelimiterExpr -> String
delimiterExtractAll (DelimiterAll s) = s

delimiterExtractClass :: DelimiterExpr -> String
delimiterExtractClass (DelimiterClass s arg) = s

delimiterExtractBindpats :: DelimiterExpr -> [Bindpat]
delimiterExtractBindpats (DelimiterBindpats ps) = ps

type DelimiterReader = Int -> FilePath -> [Lex] -> (DelimiterExpr,Int,[Lex])

delimiterAll :: DelimiterReader
delimiterAll ln filename xs = case get xs of
                               (_,LexAll s,xs0) -> (DelimiterAll s,ln,xs0)
                               _                -> error' ln filename "Expected an all cap"

delimiterClass :: DelimiterReader
delimiterClass ln filename xs = case get xs of
                                  (_,LexCap s,xs0) -> let (args,ln1,xs1) = readTypeArgs ln filename xs0
                                                      in (DelimiterClass s args,ln1,xs1)
                                  _                -> error ln filename "Expected class name"

delimiterBindpats :: DelimiterReader
delimiterBindpats ln filename xs = let (pats,ln0,xs0) = readBindpatTokens ln filename xs
                                   in (DelimiterBindpats pats,ln0,xs0)

data ListExpr = ListType Type
              | ListName String

listExtractType :: ListExpr -> Type
listExtractType (ListType t) = t

listExtractName :: ListExpr -> String
listExtractName (ListName s) = s

type ListReader = Int -> FilePath -> [Lex] -> (ListExpr,Int,[Lex])

listType :: ListReader
listType ln filename xs = let (tp,ln0,xs0) = readAType ln filename xs
                          in (ListType tp,ln0,xs0)

listName :: ListReader
listName ln filename xs = let (s,xs0) = readName ln filename xs
                          in (ListName s,ln,xs0)

listLibName :: ListReader
listLibName ln filename xs = let (s,xs0) = readLibName
                             in (ListName s,ln,xs0)
  where
  readLibName :: (String,[Lex])
  readLibName = case get xs of
                  (_,LexAll s,xs0)  -> (s,xs0)
                  (_,LexCap s,xs0)  -> (s,xs0)
                  (_,LexName s,xs0) -> (s,xs0)
                  (_,LexInt i,xs0)  -> if i >= 0 then show i
                                       else goError
                  _                 -> goError
    where
    goError = error' ln filename "Expected a library name"

data CondExpr = CondType Type
              | CondBindPat BindPat
              | CondExpr Expr

type CondReader = Int -> FilePath -> [Lex] -> (CondExpr,Int,[Lex])

type Class = (String,[Type])

data Field = Field Type String (Maybe Expr)

data Param = Param BindPat (Maybe Expr)

data ReadMode = Norm | Delimiter | SeqBuild

parse :: [Lex] -> [Top]
parse xs = case xs of
              (SetFileName s:xs0) -> recTop 1 s xs0
              _                   -> error "Expected filename setter for parse"

recTop :: Int -> FilePath -> [Lex] -> [Top]
recTop ln filename xs =
  case xs of
    (SetCol 1:_)        -> let (pat,ln0,xs0) = parseTop ln filename xs
                           in pat : recTop ln0 filename xs0
    (SetFileName s:xs0) -> recTop 1 s xs0
    (SetLine ln0:xs0)   -> recTop ln0 filename xs0
    []                  -> []
    _                   -> error' ln filename "Top-level expressions must start on column 1"

parseTop :: Int -> FilePath -> [Lex] -> (Top,Int,Lex)
parseTop ln filename xs =
  case get xs of
    (1,Keyword "synonym",xs0) -> readSynonym ln filename xs0
    (1,Keyword "type",xs0)    -> readType ln filename xs0
    (1,Keyword "struct",xs0)  -> readStruct ln filename xs0
    (1,Keyword "union",xs0)   -> readUnion ln filename xs0
    (1,Keyword "enum",xs0)    -> readEnum ln filename xs0
    (1,Keyword "class",xs0)   -> readClass ln filename xs0
    (1,Keyword "use",xs0)     -> readUse ln filename xs0
    _                         -> readBind ln filename xs

readSynonym :: TopReader
readSynonym ln filename xs = let (nm,xs0)     = readCap ln filename xs
                                 (vars,xs1)   = readVars ln filename xs0
                                 xs2          = readEquals ln filename xs1
                                 (tp,ln3,xs3) = readAType ln filename xs2
                             in (TopSynonym nm vars tp,ln3,xs3)

readType :: TopReader
readType ln filename xs = let (nm,xs0)     = readCap ln filename xs
                              (vars,xs1)   = readVars ln filename xs0
                              xs2          = readEquals ln filename xs1
                              (tp,ln3,xs3) = readAType ln filename xs2
                          in (TopType nm vars tp,ln3,xs3)

readStruct :: TopReader
readStruct ln filename xs = let (classes,ln0,xs0) = readClasses ln filename xs
                                (nm,xs1)          = readCap ln0 filename xs0
                                (vars,xs2)        = readVars ln0 filename xs1
                                (fields,ln3,xs3)  = readFields ln0 filename xs2
                            in (TopStruct classes nm vars fields,ln3,xs3)

readUnion :: TopReader
readUnion ln filename xs = let (nm,xs0)     = readCap ln filename xs
                               (vars,xs1)   = readVars ln filename xs0
                               xs2          = readEquals ln filename xs1
                               (ls,ln3,xs3) = readList listType ln filename xs2
                           in if length ls < 2 then error' ln filename "Less than two members in union"
                              else (TopUnion nm vars $ map listExtractType ls,ln3,xs3)

readEnum :: TopReader
readEnum ln filename xs = let (nm,xs0)     = readCap ln filename xs
                              (vars,xs1)   = readVars ln filename xs0
                              xs2          = readEquals ln filename xs1
                              (ls,ln3,xs3) = readList listName ln filename xs2
                          in (TopEnum nm vars $ map listExtractName ls,ln3,xs3)

readClass :: TopReader
readClass ln filename xs = let (inherit,ln0,xs0) = readClasses ln filename xs
                               (nm,xs1)          = readCap ln0 filename xs0
                               (vars,xs2)        = readVars ln0 filename xs1
                               (fields,ln3,xs3)  = readFields ln0 filename xs2
                           in (TopClass inherit nm vars fields,ln3,xs3)

readVars :: Int -> FilePath -> [Lex] -> ([String],[Lex])
readVars ln filename xs = case get xs of
                            (_,LexAll s,xs0)  -> ([s],xs0)
                            (_,Punct '(',xs0) -> let (ds,_,xs1) = readDelimiterComma ')' delimiterAll ln filename xs0
                                                 in if null ds then error' ln filename "Empty type variable list"
                                                    else (map delimiterExtractAll ds,xs1)
                            _                 -> ([],xs)

readFields :: Int -> [Lex] -> ([Field],Int,[Lex])
readFields ln xs = if comesField
                   then let (ln0,xs0)        = readNewline ln filename xs
                            (field0,ln1,xs1) = readField ln0 xs0
                            (rest,ln2,xs2)   = readFields ln1 filename xs1
                        in (field0 : rest,ln2,xs2)
                   else ([],ln,xs)
  where
  comesField :: Bool
  comesField = case get xs of
                 (_,SetLine _,xs0) -> getCol xs0 == 4
                 _                 -> False
  readField :: Int -> [Lex] -> (Field,Int,[Lex])
  readField ln xs = let (tp,ln0,xs0)  = readAType ln filename xs
                        (nm,xs1)      = readName ln0 filename xs0
                        (def,ln2,xs2) = case get xs1 of
                                          (_,LexKeyword "=",xs2) -> let (exp,ln3,xs3) = readExpr Norm (Left 0) ln0 filename xs1
                                                                    in (Just exp,ln3,xs3)
                                          _                      -> (Nothing,ln0,xs1)
                    in (Field tp nm def,ln2,xs2)

readClasses :: Int -> FilePath -> [Lex] -> ([Class],Int,[Lex])
readClasses ln filename xs =
  let (classes,ln0,xs0) = case get xs of
                            (_,LexCap s,xs0)  -> let (args,ln1,xs1) = readTypeArgs ln filename xs0
                                                 in ([(s,args)],ln1,xs1)
                            (_,Punct '(',xs0) -> let (ds,ln1,xs1) = readDelimiterComma ')' delimiterClass ln filename xs0
                                                 in if null ds then error' ln filename "Empty class list"
                                                    else (map delimiterExtractClass ds,ln1,xs1)
                            _                 -> ([],ln,xs)
      xs1 | null classes = xs0
          | otherwise    = case get xs0 of
                             (_,LexKeyword ">>",xs1) -> xs1
                             _                       -> error' ln0 filename "Expected right arrows"
  in (classes,ln0,xs1)

readList :: ListReader -> Int -> FilePath -> [Lex] -> ([ListExpr],Int,[Lex])
readList fn ln filename xs = let (e0,ln0,xs0) = fn ln filename xs
                             in case get xs0 of
                                  (_,Punct ',',xs1) -> let (rest,ln2,xs2) = recComma ln0 xs1
                                                       in (e0 : rest,ln2,xs2)
                                  (_,SetLine _,xs1) -> let (rest,ln2,xs2) = recNewline ln0 xs1
                                                       in (e0 : rest,ln2,xs2)
                                  _                 -> ([e0],ln0,xs0)

readUse :: TopReader
readUse ln filename xs = let (ls,ln0,xs0) = readList listLibName ln filename xs
                         in (TopUse $ map listExtractName ls,ln0,xs0)

readBind :: TopReader
readBind ln filename xs =
  let (keywords,xs0)    = getKeywords xs
      (bindpat,ln1,xs1) = readBindpat ln filename xs0
      (exp,ln2,xs2)     = case get xs1 of
                            (_,Keyword "=",xs2) -> readExpr Norm (Left 0) ln1 filename xs2
                            (_,SetLine ln2,xs2) -> readExpr Norm (Left 1) ln2 filename xs2
                            _                   -> error' ln1 filename "Expected equals sign or newline"
  in (TopBind keywords bindpat exp,ln2,xs2)
  where
  getKeywords :: [Lex] -> ([String],[Lex])
  getKeywords xs = case get xs of
                     (_,LexKeyword s,xs0) -> if s `elem` ["infixl","infixr","local","parallel"]
                                             then let (rest,xs1) = getKeywords xs0
                                                  in (s : rest,xs1)
                                             else ([],xs)
                     _                    -> ([],xs)

readBindpat :: Int -> FilePath -> [Lex] -> (Bindpat,Int,[Lex])
readBindpat ln filename xs = let (bindpats,ln0,xs0) = readBindpatTokens ln filename xs
                             in (compileBindpat bindpats,ln0,xs0)
  where
  compileBindpat :: [Bindpat] -> Bindpat
  compileBindpat ps =
    case ps of
      

readBindpatTokens :: Int -> FilePath -> [Lex] -> ([Bindpat],Int,[Lex])
readBindpatTokens ln filename xs = let (t0,ln0,xs0) = readBindpatToken
                                   in case t0 of
                                        Just t0' -> let (rest,ln1,xs1) = readBindpatTokens ln0 filename xs
                                                    in (t0' : rest,ln1,xs1)
                                        Nothing  -> ([],ln0,xs0)
  where
  readBindpatToken :: (Maybe Bindpat,Int,[Lex])
  readBindpatToken =
    | comesType xs = let (t,ln0,xs0) = readAType ln filename xs
                     in (Just $ BindType t,ln0,xs0)
    | otherwise    = case get xs of
                       (_,LexKeyword "_",xs0) -> (Just BindIgnore,ln,xs0)
                       (_,LexName s,xs0)      -> (Just $ BindName s,ln,xs0)
                       (_,LexChar ':',xs0)    -> (Just $ BindOp ":",ln,xs0)
                       (_,LexOp ":",xs0)      -> (Just BindCons,ln,xs0)
                       (_,LexOp s,xs0)        -> (Just $ BindOp s,ln,xs0)
                       (_,Punct '(',xs0)      -> let (ds,ln1,xs1) = readDelimiter ')' delimiterBindpats ln filename xs0
                                                 in (Just $ BindTuple (map delimiterExtractBindpats ds) Nothing,ln1,xs1)
                       (_,Punct '[',xs0)      -> let (ds,ln1,xs1) = readDelimiter ']' delimiterBindpats ln filename xs0
                                                 in (Just $ BindArray (map delimiterExtractBindpats ds) Nothing,ln1,xs1)
                       (_,Punct '{',xs0)      -> let (ds,ln1,xs1) = readDelimiter '}' delimiterBindpats ln filename xs0
                                                 in (Just $ BindList (map delimiterExtractBindpats ds) Nothing,ln1,xs1)
                       _                      -> (Nothing,ln,xs)

readTypeArgs :: Int -> FilePath -> [Lex] -> ([Type],Int,[Lex])
readTypeArgs ln filename xs =
  case get xs of
    (_,Punct '(',xs0) -> let (ds,ln1,xs1) = readDelimiter ')' delimiterType ln filename xs0
                         in if null ds then error' ln filename "Empty type argument list"
                            else (map delimiterExtractType ds,ln1,xs1)
    _                 -> if comesType xs
                         then let (tp,ln0,xs0) = readAType ln filename xs
                              in ([tp],ln0,xs0)
                         else ([],ln,xs)

skipOver :: Char -> Int -> FilePath -> [Lex] -> (Int, [Lex])
skipOver p ln filename xs = case get xs of
                              (_,Punct '(',xs0)   -> let (ln1,xs1) = skipOver ')' ln filename xs0
                                                     in skipOver ln1 p xs1
                              (_,Punct '[',xs0)   -> let (ln1,xs1) = skipOver ']' ln filename xs0
                                                     in skipOver ln1 p xs1
                              (_,Punct '{',xs0)   -> let (ln1,xs1) = skipOver '}' ln filename xs0
                                                     in skipOver p ln filename xs1
                              (_,Punct p0,xs0)    -> if p0 == p
                                                     then (ln,xs0)
                                                     else skipOver p ln filename xs0
                              (_,SetLine ln0,xs0) -> skipOver p ln0 filename xs0
                              (_,EOF,_)           -> error' ln filename "End of file within delimiter"

readCloseParen :: Int -> FilePath -> [Lex] -> [Lex]
readCloseParen ln filename xs = case get xs of
                                  (_,Punct ')',xs0) -> xs0
                                  (_,_,_)           -> error' ln filename "Expected a close paren"

readOp :: Int -> FilePath -> [Lex] -> (String,[Lex])
readOp ln filename xs = case get xs of
                          (_,Op s,xs0) -> (s,xs0)
                          _            -> error' ln filename "Expected an operator name"

comesPipe :: Int -> Int -> FilePath -> [Lex] -> Bool
comesPipe indent ln filename xs = case get xs of
                                    (_,Op "|",_)      -> True
                                    (_,Punct '(',xs0) -> let (ln1,xs1) = skipOver ln ')' xs0
                                                         in comesPipe indent ln1 xs1
                                    (_,Punct '[',xs0) -> let (ln1,xs1) = skipOver ln ']' xs0
                                                         in comesPipe indent ln1 xs1
                                    (_,Punct '{',xs0) -> let (ln1,xs1) = skipOver ln '}' xs0
                                                         in comesPipe indent ln1 xs1
                                    (_,Punct p,xs0)   -> if p `elem` punctTerminators then False
                                                         else comesPipe indent ln filename xs0
                                    (_,EOF,_)         -> error' ln filename "File ended in delimiter"
                                    (_,SetLine _,xs0) -> case get xs0 of
                                                           (c,Op "|",_) -> c == indent
                                                           _            -> False
                                    (_,_,xs0)         -> comesPipe indent ln filename xs0

readList :: Int -> Int -> FilePath -> [Lex] -> (Expr,Int,[Lex])
readList indent ln filename xs
  | comesPipe indent ln filename xs =
      let (exp,ln0,xs0) = readExpr SeqBuild ln filename xs
      in case get xs0 of
           (_,Op "|",xs1)      -> let (vars,ln2,xs2) = readSBVars 0 ln0 filename xs1
                                      xs3            = readEndBracket ln2 filename xs2
                                  in (makeSB exp vars, ln2, xs3)
           (_,SetLine ln1,xs1) -> let (vars,ln2,xs2) = readSBVars indent ln1 filename xs1
                                      xs3            = readEndBracket ln2 filename xs1
                                  in (makeSB exp vars, ln2, xs3)
           _                   -> error' ln0 filename "Expected a pipe"
  | otherwise = let (exps,ln1,xs1) = readDelimiter '}'
                                                   (readExpr Delimiter (Left 0))
                                                   ln
                                                   filename
                                                   xs0
                in (makeList exps, ln1, xs1)

readSBVars :: Int -> Int -> FilePath -> [Lex] -> ((),Int,[Lex])
readSBVars indent ln filename xs =

readModifier :: Int -> FilePath -> [Lex] -> (Expr,Int,[Lex])
readModifier ln filename xs =
  let (key,ln0,xs0) = readSub Nothing ln filename xs
      (ln1,xs1)     = readEquals ln0 filename xs0
      (exp,ln2,xs2) = readExpr Norm (Left 0) ln1 filename xs1
  in (Modifier key exp, ln2, xs2)

readNewlineMaybe :: Int -> [Lex] -> (Int,[Lex])
readNewlineMaybe ln xs = case get xs of
                           (_,SetLine ln0,xs0) -> (ln0,xs0)
                           _                   -> (ln,xs)

readAtom :: Int -> FilePath -> [Lex] -> (Maybe Expr,Int,[Lex])
readAtom ln filename xs =
  let (binds,ln0,xs0) = readBinds (getCol xs) ln xs
  in if null binds
     then let (c,t,xs1) = get xs0
              (a0,ln2,xs2) = case t of
                               LexOp s             -> (Just $ ExprOp s,ln0,xs1)
                               LexKeyword "case"   -> readCase c ln0 xs1
                               LexKeyword "do"     -> readDo ln0 xs1
                               LexKeyword "each"   -> readEach c ln0 xs1
                               LexKeyword "for"    -> readFor c ln0 xs1
                               LexKeyword "if"     -> readIf c ln0 xs1
                               LexKeyword "lambda" -> readLambda c ln0 xs1
                               LexKeyword "match"  -> readMatch c ln0 xs1
                               LexKeyword "modify" -> readModify c ln0 xs1
                               LexKeyword "pure"   -> readPure ln0 xs1
                               LexKeyword "range"  -> readRange c ln0 xs1
                               LexKeyword "switch" -> readSwitch c ln0 xs1
                               LexKeyword "tcase"  -> readTCase c ln0 xs1
                               LexKeyword "until"  -> readUntil c ln0 xs1
                               LexKeyword "while"  -> readWhile c ln0 xs1
                               LexChar c           -> (Just $ ExprChar c,ln0,xs1)
                               LexInt i            -> (Just $ ExprInt i,ln0,xs1)
                               LexKeyword "_"      -> (Just ExprDefault,ln0,xs1)
                               LexName s           -> (Just $ ExprName s,ln0,xs1)
                               LexString s         -> (Just $ ExprString s,ln0,xs1)
                               LexHash i           -> (Just $ ExprHash i,ln0,xs1)
                               Punct '\''          -> let (exp,ln1,xs1) = readSub ln filename xs1
                                                      in (mkFuncall (mkName "quote") exp,ln1,xs1)
                               LexPunct '('        -> let (exps,ln2,xs2) = readDelimiter ')'
                                                                                         (readExpr Delimiter (Left 0))
                                                                                         ln0
                                                                                         filename
                                                                                         xs1
                                                      in (Just $ ExprTuple exps,ln2,xs2)
                               LexPunct '['        -> let (exps,ln2,xs2) = readDelimiter ']'
                                                                                         (readExpr Delimiter (Left 0))
                                                                                         ln0
                                                                                         filename
                                                                                         xs1
                                                      in (Just $ ExprArray exps,ln2,xs2)
                               LexPunct '{'        -> let (exps,ln2,xs2) = readDelimiter '}'
                                                                                         (readExpr Delimiter (Left 0))
                                                                                         ln0
                                                                                         filename
                                                                                         xs1
                                                      in (Just $ ExprList exps,ln2,xs2)
                               LexLoop name        -> readLoop name c ln0 xs1
                               _                   -> (Nothing,ln0,xs0)
          in case a0 of
               Nothing  -> (a0,ln2,xs2)
               Just a0' -> case get xs of
                             (_,Keyword "as",xs3)   -> readAs exp ln2 xs3
                             (_,Keyword "from",xs3) -> readFrom exp ln2 xs3
                             (_,Keyword "the",xs3)  -> readThe exp ln2 xs3
                             _                      -> (a0,ln2,xs2)
     else let (exp,ln1,xs1) = readExpr Norm (Right $ getCol xs) ln0 filename xs0
          in (Let binds exp,ln1,xs1)
  where
  readBinds :: Int -> Int -> [Lex] -> ([(BindPat,Expr)],Int,[Lex])
  readBinds indent ln xs
    | getCol xs == indent && comesEquals xs =
        let (bindpat,ln0,xs0) = readBindpat ln filename xs
            (ln1,xs1)         = readNewlineMaybe ln0 xs0
            (exp,ln2,xs2)     = readExpr Norm (Right indent) ln1 filename xs1
            (rest,ln3,xs3)    = readBinds indent ln2 xs2
        in ((bindpat,exp) : rest,ln3,xs3)
    | otherwise      = ([],ln,xs)
    where
    comesEquals :: Int -> [Lex] -> Bool
    comesEquals ln xs = case get xs of
                          (_,Keyword "=",_) -> True
                          (_,Punct '(',xs0) -> let (ln1,xs1) = skipOver ')' ln filename xs0
                                               in comesEquals ln1 xs1
                          (_,Punct '[',xs0) -> let (ln1,xs1) = skipOver ']' ln filename xs0
                                               in comesEquals ln1 xs1
                          (_,Punct '{',xs0) -> let (ln1,xs1) = skipOver '}' ln filename xs0
                                               in comesEquals ln1 xs1
                          (_,SetLine _,_)   -> False
                          (_,Keyword _,_)   -> False
                          (_,EOF,_)         -> False
                          (_,_,xs0)         -> comesEquals ln xs0

  readAs :: Expr -> Int -> [Lex] -> (Maybe Expr,Int,[Lex])
  readAs exp ln xs =
    let (tp,ln0,xs0) = readType ln filename xs
    in (Just $ ExprAs tp exp, ln0, xs0)

  readCase :: Int -> Int -> [Lex] -> (Maybe Expr,Int,[Lex])
  readCase indent ln xs =
    let (exp,ln0,xs0) = readExpr 0 (Left 0) ln filename xs
        (ln1,xs1)     = readNewline ln0 filename xs0
        (cl,ln1,xs1)  = readClauses indent (readExpr 0 (Left 0)) "->" ln1 filename xs1
    in (Just $ ExprCase exp cl,ln1,xs1)

  readDo :: Int -> [Lex] -> (Maybe Expr,Int,[Lex])
  readDo ln xs =

  readEach :: Int -> Int -> [Lex] -> (Maybe Expr,Int,[Lex])
  readEach indent ln xs =

  readFor :: Int -> Int -> [Lex] -> (Maybe Expr,Int,[Lex])
  readFor indent ln xs =

  readFrom :: Expr -> Int -> [Lex] -> (Maybe Expr,Int,[Lex])
  readFrom exp ln xs =
    let (name,ln0,xs0) = readCap ln filename xs
    in (Just $ ExprFrom name exp, ln0, xs0)

  readIf :: Int -> Int -> FilePath -> [Lex] -> (Expr,Int,[Lex])
  readIf indent ln filename xs =
    let (cl,ln1,xs1) = readClauses indent (readExpr 0 (Left 0)) "->" ln0 filename (SetLine ln : xs0)
    in (Just $ ExprIf cl,ln2,xs2)

  readLambda :: Int -> Int -> [Lex] -> (Maybe Expr,Int,[Lex])
  readLambda indent ln xs =
    let (pat,ln0,xs0)    = readBindpat ln filename xs
        (body,ln1,xs1)   = case get xs0 of
                             (_,Keyword "->",xs1) -> readExpr Norm (Left 0) ln0 filename xs1
                             (_,SetLine _,xs1)    -> readExpr Norm (Left indent) ln0 filename xs1
                             _                    -> error' ln0 filename "Expected arrow or newline"
    in (Just $ ExprLambda pat body,ln1,xs1)

  readLoop :: String -> Int -> Int -> [Lex] -> (Maybe Expr,Int,[Lex])
  readLoop name indent ln xs =
    let (rt,ln0,xs0)   = readType ln filename xs
        (ln1,xs1)      = readOpenParen ln0 filename xs0
        (lps,ln2,xs2)  = readDelimiter ')' readLoopParam ln1 filename xs1
        (ln3,xs3)      = readNewline ln2 filename xs2
        (body,ln4,xs4) = readExpr [] (Left indent) ln3 filename xs3
        params         = map loopParamParam lps
        args           = Tuple $ map loopParamArg lps
    in (ExprLoop [(makeType params, name, Function params body)]
            (Funcall name args)
      , ln4
      , xs4)
    where
    makeType :: Expr -> Expr -> Expr
    makeType rt params = FN rt (Tuple $ map paramType params)

  readMatch :: Int -> Int -> [Lex] -> (Maybe Expr,Int,[Lex])
  readMatch indent ln xs =
    let (exp,ln0,xs0) = readExpr Norm (Left 0) ln filename xs
        (cl,ln1,xs1)  = readClauses readBindpat "->" ln0 filename xs0
    in (Just $ ExprMatch exp cl,ln1,xs1)

  readModify :: -> Int -> Int -> FilePath -> [Lex] -> (Maybe Expr,Int,[Lex])
  readModify indent ln filename xs =
    let (exp,ln0,xs0) = readExpr Norm (Left 0) ln filename xs
        (cl,ln1,xs1)  = readCl ln0 xs0
    in (Just $ ExprModify exp cl,ln1,xs1)
    where
    readCl :: Int -> [Lex] -> ([(CondExpr,Expr)], Int, [Lex])
    readCl ln xs =
      case get xs of
        (_,Punct '(',xs0) -> let (mods,ln1,xs1) = readDelimiter ')' readModifier ln filename xs0
                                 mods'          = transformModifier mods
                             in (mods',ln1,xs1)
        _                 -> let (mods,_,ln0,xs0) = readClauses False 
                                                                (readClause False readSub "=")
                                                                []
                                                                indent
                                                                ln
                                                                filename
                                                                xs
                             in (mods,ln0,xs0)

  readPure :: Int -> [Lex] -> (Maybe Expr,Int,[Lex])
  readPure ln xs =
    let (exp,ln0,xs0) = readExpr Norm (Left 0) ln filename xs
    in (Just $ ExprPure exp,ln0,xs0)

  readRange :: Int -> Int -> [Lex] -> (Maybe Expr,Int,[Lex])
  readRange indent ln xs =
    let (exp,ln0,xs0) = readExpr Norm (Left 0) ln filename xs
        (ln1,xs1)     = readNewline ln0 filename xs0
        (cl,ln1,xs1)  = readClauses indent readSub "->" ln0 filename xs0
    in (Just $ ExprRange cl,ln1,xs1)

  readSwitch :: Int -> Int -> [Lex] -> (Maybe Expr,Int,[Lex])
  readSwitch indent ln xs =
    let (exp,ln0,xs0) = readExpr Norm (Left 0) ln filename xs
        (cl,ln1,xs1)  = readClauses indent readSub "->" ln0 filename xs0
    in (Just $ ExprSwitch exp cl,ln1,xs1)

  readTCase :: Int -> Int -> [Lex] -> (Maybe Expr,Int,[Lex])
  readTCase indent ln xs =
    let (exp,ln0,xs0)    = readExpr Norm (Left 0) ln filename xs
        (cl,ln1,xs1) = readClauses indent readType "->" ln0 filename xs0
    in (Just $ ExprTCase exp cl,ln1,xs1)

  readThe :: Expr -> Int -> [Lex] -> (Maybe Expr,Int,[Lex])
  readThe exp ln xs =
    let (tp,ln0,xs0) = readAType ln filename xs
    in (Just $ ExprThe tp exp,ln0,xs0)

  readUntil :: Int -> Int -> [Lex] -> (Maybe Expr,Int,[Lex])
  readUntil indent ln xs =

  readWhile :: Int -> Int -> [Lex] -> (Maybe Expr,Int,[Lex])
  readWhile indent ln xs =

transformModifier :: [Expr] -> [(Expr,Expr)]
transformModifier (Modifier key exp:xs) = (key,exp) : transformModifier xs
transformModifier []                    = []

tList :: Expr -> [Expr]
tList (Tuple tks) = tks
tList (TU tks)    = tks
tList tk          = [tk]

keywordTerminators :: [String]
keywordTerminators = ["as","from","the","->","<-",">>"]

punctTerminators :: [Char]
punctTerminators = [',',')',']','}']

readCap :: Int -> FilePath -> [Lex] -> (String,[Lex])
readCap ln filename xs = case get xs of
                            (_,Cap s,xs0) -> (s,xs0)
                            _             -> error' ln filename "Expected a cap name"

readOpenParen :: Int -> FilePath -> [Lex] -> [Lex]
readOpenParen ln filename xs = case get xs of
                                 (_,Punct '(',ln0,xs0)) = xs0
                                 _                      = error' ln filename "Expected an open paren"

readClauses :: Int -> CondReader -> String -> Int -> FilePath -> [Lex] -> ([(Cond,Expr)],Int,[Lex])
readClauses indent fn arrow ln filename xs = 
  let (cond,ln0,xs0) = fn ln filename xs
  in case get xs0 of
       (_,Keyword s,xs1) -> if s == arrow
                            then let (exp,ln1,xs1)  = readExpr 0 (Left 0) ln0 filename xs0
                                     (ln2,xs2)      = comesAnother ln1 xs1
                                     (rest,ln3,xs3) = if ln2 == ln1 then ([],ln1,xs1)
                                                      else readClauses indent fn arrow ln2 filename xs2
                                 in ((cond,exp) : rest,ln3,xs3)
                            else error' ln0 filename "Bad arrow"
       _                 -> error' ln0 filename "Expected arrow"
  where
  comesAnother :: Int -> [Lex] -> (Int,[Lex])
  comesAnother ln xs = case get xs of
                         (_,SetLine ln0,xs0) -> let (c,_,_) = get xs0
                                                in if c > indent then (ln0,xs0)
                                                   else (ln,xs)
                         _                   -> (ln,xs)

loopParamParam :: Expr -> Expr
loopParamParam (LoopParam t n _) = Param t n

loopParamArg :: Expr -> Expr
loopParamArg (LoopParam _ _ a) = a

readLoopParam :: Int -> FilePath -> [Lex] -> (Expr,Int,[Lex])
readLoopParam ln filename xs =
  let (tp,ln0,xs0)   = readType ln filename xs
      (name,ln1,xs1) = readName ln0 filename xs0
      (ln2,xs2)      = readEquals ln1 filename xs1
      (exp,ln3,xs3)  = readExpr 0 (Left 0) ln2 filename xs2
  in (LoopParam tp name exp, ln3, xs3)

readName :: Int -> FilePath -> [Lex] -> (String,[Lex])
readName ln filename xs = case get xs of
                            (_,LexName s,xs0) -> (s,xs0)
                            _                 -> error' ln filename "Expected a name"

readEndSquare :: Int -> FilePath -> [Lex] -> [Lex]
readEndSquare ln filename xs = case get xs of
                                 (_,Punct ']',xs0) = xs0
                                 _                 = error' ln filename "Expected a closing square bracket"

skipAhead :: Int -> [Lex] -> (Int,[Lex])
skipAhead ln [] = (ln,[])
skipAhead ln xs = case head xs of
                    SetLine ln0   -> skipAhead ln0 (tail xs)
                    SetCol 1      -> (ln,xs)
                    SetFileName _ -> (ln,xs)
                    _             -> skipAhead ln (tail xs)

get :: [Lex] -> (Int,Lex,[Lex])
get (SetCol c:t:xs) = (c,t,xs)
get (SetLine ln:xs) = recNewline ln xs
  where
  recNewline :: Int -> [Lex] -> (Int,Lex,[Lex])
  recNewline _  (SetLine ln:xs) = recNewline ln xs
  recNewline ln xs              = (0,SetLine ln,xs)
get xs = (0,EOF,xs)

getCol :: [Lex] -> Int
getCol (SetCol c:_) = c
getCol _            = 0

readEquals :: Int -> FilePath -> [Lex] -> [Lex]
readEquals ln filename xs = case get xs of
                              (_,Keyword "=",xs0) -> xs0
                              _                   -> error' ln filename "Expected an equals sign"

readNewline :: Int -> FilePath -> [Lex] -> (Int,[Lex])
readNewline ln filename xs = case get xs of
                               (_,SetLine ln0,xs0) -> (ln0,xs0)
                               _                   -> error' ln filename "Expected newline"

readEndBracket :: Int -> FilePath -> [Lex] -> [Lex]
readEndBracket ln filename xs = case get xs of
                                  (_,Punct '}',xs0) -> xs0
                                  _                 -> error' ln filename "Expected end bracket"

readDelimiter :: Char -> DelimiterReader -> Int -> FilePath -> [Lex] -> ([DelimiterExpr],Int,[Lex])
readDelimiter end fn ln filename xs =
  case get xs of
    (c,Punct p,xs0)     -> if p == end
                           then ([],ln,xs0)
                           else go c
    (_,SetLine ln0,xs0) -> let (c,_,_) = get xs0
                           in readDelimiterNewline c end fn ln0 filename xs0
    (c,_,_)             -> go c
  where
  go :: Int -> ([DelimiterExpr],Int,[Lex])
  go indent = let (t0,ln0,xs0) = fn ln filename xs
              in case get xs0 of
                   (_,Punct p,xs1)     -> if p == end
                                          then ([t0],ln0,xs1)
                                          else if p == ','
                                               then let (rest,ln2,xs2) = readDelimiterComma end fn ln0 filename xs1
                                                    in (t0 : rest,ln2,xs2)
                                               else error' ln0 filename "Unexpected punctuation"
                   (_,SetLine ln1,xs1) -> readDelimiterNewline indent end fn ln1 filename xs1
                   _                   -> error' ln1 filename "Expected comma, newline or end punctuation"

readDelimiterComma :: Char -> DelimiterReader -> DelimiterListReader
readDelimiterComma end fn ln filename xs =
  let (t0,ln0,xs0) = fn ln filename xs
  in case get xs0 of
       (_,Punct p,xs1) -> if p == ','
                          then let (rest,ln2,xs2) = readDelimiterComma end fn ln0 filename xs1
                               in (t0 : rest,ln2,xs2)
                          else if p == end
                               then ([tk],ln0,xs1)
                               else error' ln0 filename "Unexpected punctuation"
       _               -> error' ln0 filename "Expected end punctuation or comma"

readDelimiterNewline :: Int -> Char -> DelimiterReader -> DelimiterListReader
readDelimiterNewline indent end fn ln filename xs =
  case get xs of
    (c,Punct p,xs0) -> if p == end
                       then ([],ln,xs0)
                       else go c
    (c,_,_)         -> go c
  where
  go :: Int -> ([Expr],Int,[Expr])
  go c = if c == indent
         then let (t0,ln0,xs0) = fn ln filename xs
              in case get xs0 of
                   (_,Punct p,xs1)   -> if p == end
                                        then ([tk],ln0,xs1)
                                        else error' ln0 filename "Expected newline or end punctuation"
                   (_,SetLine _,xs1) -> readDelimiterNewline indent end fn ln1 filename xs1
         else error' ln filename "Bad indentation"

readAType :: Int -> FilePath -> [Lex] -> (Type,Int,[Lex])
readAType ln filename xs
  | comesType xs = let (mems,ln0,xs0) = readTypeMembers ln filename xs
                       tp | length mems == 1 = head mems
                          | otherwise        = UN mems
                   in (tp,ln0,xs0)
  | otherwise    = (DR,ln,xs)

readTypeSubSub :: Int -> FilePath -> [Lex] -> (Type,Int,[Lex])
readTypeSubSub ln filename xs =
  case get xs of
    (_,Cap name,xs0)    -> let (arg,ln1,xs1) | comesType xs0 = readTypeSubSub ln xs0
                                             | otherwise     = (TU [],ln,xs0)
                               args = tList arg
                           in (NM name args,ln1,xs1)
    (_,LexAll s,xs0)    -> (TypeVar s,ln,xs0)
    (_,Punct '(',xs0)   -> let (tps,ln1,xs1) = readDelimiter ')' readType ln filename xs0
                           in (TU tps,ln1,xs1)
    (_,Punct '[',xs0)   -> let (tp,ln1,xs1) = readType ln filename xs0
                               (ln2,xs2)    = readEndSquare ln1 filename xs1
                           in (NM "Array" [tp], ln2, xs2)
    (_,Punct '{',xs0)   -> let (tp,ln1,xs1) = readType ln filename xs0
                               (ln2,xs2)    = readEndBracket ln1 filename xs1
                           in (NM "List" [tp],ln2,xs2)
    _                   -> error' ln filename "Expected a type"

readTypeSub :: Int -> FilePath -> [Lex] -> (Type,Int,[Lex])
readTypeSub ln filename xs =
  let (t0,ln0,xs0) = readTypeSubSub ln filename xs
  in case get xs of
       (_,Keyword "<-",xs1) -> let (t1,ln2,xs2) = readTypeSub ln0 filename xs1
                               in (FN t0 t1,ln2,xs2)
       (_,Keyword "<<",xs1) -> let (t1,ln2,xs2) = readTypeSub ln0 filename xs1
                               in (AC t0 t1,ln2,xs2)
       _                    -> (t0,ln,xs)

readTypeMembers :: Int -> FilePath -> [Lex] -> ([Type], Int, [Lex])
readTypeMembers ln filename xs =
  let (t0,ln0,xs0) = readTypeSub ln filename xs
  in case get xs0 of
       (_,Op "|",xs1) -> let (rest,ln2,xs2) = readTypeMembers ln0 filename xs1
                         in (t0 : rest,ln2,xs2)
       _              -> ([t0],ln,xs)

comesType :: [Lex] -> Bool
comesType xs = isJust $ comesTypeM xs

comesTypeM :: [Lex] -> (Maybe [Lex])
comesTypeM xs = case get xs of
                  (_,Cap _,xs0)     -> case comesTypeM xs0 of
                                         Just xs1 -> Just xs1
                                         Nothing  -> Just xs0
                  (_,Punct '(',xs0) -> readDelimiterM xs0
                  (_,Punct '[',xs0) -> comesTypeM xs0 >>= comesPunctM ']'
                  (_,Punct '{',xs0) -> comesTypeM xs0 >>= comesPunctM '}'

comesPunctM :: Char -> [Lex] -> Maybe [Lex]
comesPunctM p xs = case get xs of
                     (_,Punct p0,xs0) -> if p == p0 then Just xs0
                                         else Nothing
                     _                -> Nothing

readDelimiterM :: [Lex] -> Maybe [Lex]
readDelimiterM xs = case get xs of
                      (_,Punct ')',xs0) -> Just xs0
                      (_,SetLine _,xs0) -> let (c,_,_) = get xs0
                                           in recNewline c xs0
                      (c,_,_)           -> case comesTypeM xs of
                                             Just xs0 -> case get xs0 of
                                                           (_,Punct ')',xs1) -> Just xs1
                                                           (_,Punct ',',xs1) -> recComma xs1
                                                           (_,SetLine _,xs1) -> recNewline c xs1
                                                           _                 -> Nothing
                                             Nothing  -> Nothing
  where
  recComma :: [Lex] -> Maybe [Lex]
  recComma xs = case comesTypeM xs of
                  Just xs0 -> case get xs0 of
                                (_,Punct ')',xs1) -> Just xs1
                                (_,Punct ',',xs1) -> recComma xs1
                                _                 -> Nothing
                  Nothing  -> Nothing
  recNewline :: Int -> [Lex] -> Maybe [Lex]
  recNewline indent xs = case get xs of
                           (_,Punct ')',xs0) -> Just xs0
                           (c,_,_)           -> if c == indent
                                                then case comesTypeM xs of
                                                       Just xs0 -> case get xs0 of
                                                                     (_,Punct ')',xs1) -> Just xs1
                                                                     (_,SetLine _,xs1) -> recNewline indent xs1
                                                                     _                 -> Nothing
                                                       Nothing  -> Nothing
                                                else Nothing

 -- Error

error' :: Int -> FilePath -> String -> a
error' ln filename message = error ("Parse error in file " ++ filename ++
                                  " on line " ++ show ln ++ "\n" ++ message)
