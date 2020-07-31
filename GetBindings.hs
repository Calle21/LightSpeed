module GetBindings (skipAhead
                   ,getBindings
                   ,get
                   ,readEquals
                   ,readNewline
                   ,tList
                   ,comesType
                   ,readCap
                   ,readType
                   ,readDelimiter
                   ,error') where

import Share (Token(..), Inf(Inf))

getBindings :: [(String,Inf)] -> [Token] -> [(Token, Token, Token)]
getBindings infs xs = case xs of
                        (SetFileName s:xs') -> recTop infs 1 s xs'
                        _                   -> error "Expected filename setter for getBindings"

recTop :: [(String,Inf)] -> Int -> FilePath -> [Token] -> [(Token,Token,Token)]
recTop infs ln filename xs =
  case xs of
    (SetFileName s:xs') -> recTop infs 1 s xs'
    (SetCol 1:_)        -> if comesBinding ln filename xs
                           then let (bind,ln',xs') = readBinding infs ln filename xs
                                in bind : recTop infs ln' filename xs'
                           else let (ln',xs') = skipAhead ln xs'
                                in recTop infs ln' filename xs'
    (SetLine ln':xs')   -> recTop infs ln' filename xs'
    []                  -> []
    _                   -> error' ln filename "Top-level expressions must start on column 1"

comesBinding :: [Token] -> Bool
comesBinding xs = case get xs of
                    (_,Cap _,_)      -> True
                    (_,Punct '(', _) -> True
                    (_,Punct '[', _) -> True
                    (_,Puntc '{', _) -> True
                    _                -> False

comesOp :: Int -> FilePath -> [Token] -> Bool
comesOp ln filename xs = case xs of
                           (_,Punct '(',xs0) -> comesOp ln filename $ skipOver ln ')' xs0
                           (_,Punct '[',xs0) -> comesOp ln filename $ skipOver ln ']' xs0
                           (_,Punct '{',xs0) -> comesOp ln filename $ skipOver ln '}' xs0
                           (_,Op _,_)        -> True
                           (_,SetLine _,_)   -> False
                           (_,EOF,_)         -> False
                           (_,Keyword "=",_) -> False
  where
  skipOver :: Int -> Char -> [Token] -> [Token]
  skipOver ln p xs = case get xs of
                       (_,Punct '(',xs0)   -> skipOver ln p $ skipOver ln ')' xs0
                       (_,Punct '[',xs0)   -> skipOver ln p $ skipOver ln ']' xs0
                       (_,Punct '{',xs0)   -> skipOver ln p $ skipOver ln '}' xs0
                       (_,Punct p0,xs0)    -> if p0 == p
                                              then xs0
                                              else skipOver ln p xs0
                       (_,SetLine ln',xs0) -> skipOver ln' p xs0
                       (_,EOF,_)           -> error' ln filename "End of file within delimiter"

readPat :: Int -> FilePath -> [Token] -> (Token,Int,[Token])
readPat ln filename xs =
  case get xs of
    (_,Name s,xs0)    -> (Name s,ln,xs0)
    (_,Punct '(',xs0) -> let (pat,ln1,xs1) = if comesColon xs0
                                             then readList ln xs0
                                             else let (pats,ln1,xs1) = readDelimiter ')' readPat ln filename xs0
                                                  in (Tuple pats,ln1,xs1)
  where
  comesColon :: [Token] -> Bool
  comesColon xs = case get xs of
                    (_,Op ":",_)    -> True
                    (_,Punct ')',_) -> False
                    (_,EOF,_)       -> False
                    (_,SetLine _,_) -> False
                    (_,_,xs0)       -> comesColon xs0
  readList :: Int -> [Token] -> (Token,

readBinding :: [(String, Inf)] -> Int -> FilePath -> [Token] -> ((Token,Token),Int,[Token])
readBinding infs ln filename xs =
  let (pat,params,ln0,xs0) = if comesType xs
                             then let (rt,ln0,xs0) = readType ln filename xs
                                  in if comesOp ln0 filename xs0
                                     then readOpDef rt ln0 filename xs0
                                     else readDef rt ln0 filename xs0
                             else let (pat,ln0,xs0) = readPat ln filename xs
                                  in (pat,Nothing,ln0,xs0)
      (exp,ln1,xs1)  = case get xs0 of
                         (_,Keyword "=",xs1) -> readExpr infs (getReturnType bind) [] (Left 0) ln1 filename xs1
                         (_,Newline,xs1)     -> let (indent,_,_) = get xs
                                                in readExpr infs (Just $ getReturnType bind) [] (Left indent) ln1 filename xs1
                         _                   -> error' ln1 filename "Expected equals sign or newline"
  in (makeTypes bind
     ,makeNames bind
     ,makeVals bind exp
     ,ln1
     ,xs1)
  where
  makeVals :: Either [Token] Token -> Token -> [Token]
  makeVals (Left tns) exp = rec 1 tns
    where
    rec :: Int -> [Token] -> [Token]
    rec n (_:xs) = Funcall (Op "!") (Tuple [exp,n]) : rec (n + 1) xs
    rec _ []     = []
  makeVals (Right tnp) exp = let params = tnpParams tnp
                             in if null params
                                then [exp]
                                else [Function (tnpType tnp) params exp]
  makeTypes :: Either [Token] Token -> [Token]
  makeTypes (Left tns) = map tnType tns
  makeTypes (Right tnp) = [tnpToType tnp]
  makeNames :: Either [Token] Token -> [String]
  makeNames (Left tns) = map tnName tns
  makeNames (Right tnp) = [tnpName tnp]
  getReturnType :: Either [Token] Token -> Token
  getReturnType (Left tns)  = TU $ map tnType tns
  getReturnType (Right tnp) = tnpType tnp
  tnpToType :: Token -> Token
  tnpToType (TNP t _ p) | null p    = t
                        | otherwise = FN t (TU $ map paramType p)

readTypeAndName :: Int -> FilePath -> [Token] -> (Token,Int,[Token])
readTypeAndName ln filename xs =
  let (tp,ln0,xs0) = readType ln filename xs
      (nm,ln1,xs1) = readName ln0 filename xs0
  in (TypeAndName tp nm, ln1, xs1)

tnName :: Token -> String
tnName (TypeAndName _ s) = s

tnType :: Token -> Token
tnType (TypeAndName t _) = t

paramType :: Token -> Token
paramType (Param t _) = t

tnpType :: Token -> Token
tnpType (TNP t _ _) = t

tnpParams :: Token -> [Token]
tnpParams (TNP _ _ p) = p

tnpName :: Token -> String
tnpName (TNP _ nm _) = nm

readOpDef :: Token -> Int -> FilePath -> [Token] -> (Token,Maybe [Token],Int,[Token])
readOpDef rt ln filename xs = let xs0              = readOpenParen ln filename xs
                                 (param1,ln1,xs1) = readParam ln filename xs0
                                 xs2              = readCloseParen ln1 filename xs1
                                 (name,ln3,xs3)   = readOp ln1 filename xs2
                                 xs4              = readOpenParen ln3 filename xs3
                                 (param2,ln5,xs5) = readParam ln3 filename xs4
                                 xs6              = readCloseParen ln5 filename xs5
                             in (Typed rt (String name)
                               , Just [param1,param2]
                               , ln5
                               , xs6)

readDef :: Token -> Int -> FilePath -> [Token] -> (Token,Maybe [Token],Int,[Token])
readDef rt ln filename xs = let (s,xs0) = readName ln filename xs
                                (params,ln1,xs1) = readParams ln filename xs0
                            in (Typed rt (Name s)
                              , params
                              , ln1
                              , xs1)

readTNP :: Int -> FilePath -> [Token] -> (Token,Int,[Token])
readTNP ln filename xs =
  let (rt,ln0,xs0)        = readType ln filename xs
      (nm,params,ln1,xs1) = readNameAndParams ln0 xs0
  in (TNP rt nm params, ln1, xs1)
  where
  readNameAndParams :: Int -> [Token] -> (String,[Token],Int,[Token])
  readNameAndParams ln xs =
    case get xs of
      (_,Name s,xs0)    -> let (params,ln1,xs1) = readParams ln0 filename xs0
                           in (s,params,ln1,xs1)
      (_,Punct '(',xs0) -> let (param1,ln1,xs1) = readParam ln0 filename xs0
                               xs2              = readCloseParen ln1 filename xs1
                               (name,ln3,xs3)   = readOp ln1 filename xs2
                               xs4              = readOpenParen ln3 filename xs3
                               (param2,ln5,xs5) = readParam ln3 filename xs4
                               xs6              = readCloseParen ln5 filename xs5
                           in (name,[param1,param2],ln5,xs6)
      _                 -> error' ln filename "Expected name or open paren"

readCloseParen :: Int -> FilePath -> [Token] -> [Token]
readCloseParen ln filename xs = case get xs of
                                  (_,Punct ')',xs0) -> xs0
                                  (_,_,_)           -> error' ln filename "Expected a close paren"

readOp :: Int -> FilePath -> [Token] -> (String,[Token])
readOp ln filename xs = case get xs of
                          (_,Op s,xs0) -> (s,xs0)
                          _            -> error' ln filename "Expected an operator name"

readParams :: Int -> FilePath -> [Token] -> (Maybe [Token],Int,[Token])
readParams ln filename xs =
  case get xs of
    (_,Keyword "=",_) = (Nothing,ln,xs)
    (_,Newline,_)     = (Nothing,ln,xs)
    (_,Punct '(',xs0) = let (params,ln1,xs1) = readDelimiter ')' readParam ln filename xs0
                        in (Just params, ln1, xs1)
    _                 = error' ln filename "Expected equals sign, newline or parameter list"

readParam :: Int -> FilePath -> [Token] -> (Token,Int,[Token])
readParam ln filename xs =
  let (tp,ln0,xs0) = readType ln filename xs
      (nm,ln1,xs1) = readName ln0 filename xs0
  in (Param tp nm,ln1,xs1)

readLambda :: [(String,Inf)] -> Int -> Int -> FilePath -> [Token] -> (Token,Int,[Token])
readLambda infs indent ln filename xs =
  let (rt,ln0,xs0)     = readType ln filename xs
      (ln1,xs1)        = readOpenParen ln0 filename xs0
      (params,ln2,xs2) = readDelimiter ')' readParam ln1 filename xs1
      (body,ln3,xs3)   = case get xs2 of
                           (_,Keyword "->",xs3) -> readExpr infs (Just rt) [] (Left 0) ln2 filename xs3
                           (_,Newline,xs3)     -> readExpr infs (Just rt) [] (Left indent) ln2 filename xs3
                           _                   -> error' ln2 filename "Expected arrow or newline"
  in (Function rt params body, ln3, xs3)

readSub :: [(String,Inf)] -> Maybe Token -> Int -> FilePath -> [Token] -> (Token,Int,[Token])
readSub infs et ln filename xs =
  if comesType ln filename xs
  then let (tps,nms,vals,ln0,xs0) = readBinding infs ln filename xs
           (ln1,xs1)              = readNewline ln0 filename xs0
           (indent,_,_)           = get xs
           (rest,ln2,xs2)         = readExpr infs et [] (Right indent) ln1 filename xs1
       in (Let (transformLet (tps,nms,vals)) rest, ln2, xs2)
  else let (c,t,xs0) = get xs
       in case t of
            Keyword "lambda"  -> readLambda infs c ln filename xs0
            Keyword "case"    -> readCase infs et c ln filename xs0
            Keyword "tcase"   -> readTCase infs et c ln filename xs0
            Keyword "switch"  -> readSwitch infs et c ln filename xs0
            Keyword "range"   -> readRange infs et c ln filename xs0
            Keyword "modify"  -> readModifyDestroy Modify infs c ln filename xs0
            Keyword "destroy" -> readModifyDestroy Destroy infs c ln filename xs0
            Keyword "pure"    -> readPure infs et ln filename xs0
            Access _ _        -> (t,ln,xs0)
            Char _            -> (t,ln,xs0)
            Float _           -> (t,ln,xs0)
            Int _             -> (t,ln,xs0)
            Keyword "..."     -> (t,ln,xs0)
            Keyword "_"       -> (t,ln,xs0)
            Name _            -> (t,ln,xs0)
            String _          -> (t,ln,xs0)
            Hash _            -> (t,ln,xs0)
            Punct '#'         -> 
            Punct '('         -> let (exps,ln1,xs1) = readDelimiter ')'
                                                                    (readExpr infs Nothing [] (Left 0))
                                                                    ln
                                                                    filename
                                                                    xs0
                                 in (Tuple exps, ln1, xs1)
            Punct '['         -> let (exps,ln1,xs1) = readDelimiter ']'
                                                                    (readExpr infs Nothing [] (Left 0))
                                                                    ln
                                                                    filename
                                                                    xs0
                                 in (Array [length exps] exps, ln1, xs1)
            Punct '{'         -> readList infs ln filename xs0
            Loop name         -> readLoop infs et name c ln filename xs0
            _                 -> error' ln filename "Expected sub-expression"

makeList :: [Token] -> Token
makeList xs = undefined

comesPipe :: [Token] -> Bool
comesPipe xs = case get xs of
                 (_,Op "|",_)    -> True
                 (_,EOF,_)       -> False
                 (_,SetLine _,_) -> False
                 (_,Punct ',',_) -> False
                 (_,_,xs0)       -> comesPipe xs0

readList :: [(String,Inf)] -> Int -> FilePath -> [Token] -> (Token,Int,[Token])
readList infs ln filename xs
  | comesPipe xs = let 
  | otherwise    = let (exps,ln1,xs1) = readDelimiter '}'
                                                      (readExpr infs [] (Left 0))
                                                      ln
                                                      filename
                                                      xs0
                   in (makeList exps, ln1, xs1)

readPure :: [(String,Inf)] -> Maybe Token -> Int -> FilePath -> [Token] -> (Token,Int,[Token])
readPure infs et ln filename xs =
  let (exp,ln0,xs0) = readExpr infs et [] (Left 0) ln filename xs
  in (Pure exp,ln0,xs0)

readModifyDestroy :: (Token -> [(Token,Token)] -> Token)
                  -> [(String,Inf)]
                  -> Int
                  -> Int
                  -> FilePath
                  -> [Token]
                  -> (Token,Int,[Token])
readModifyDestroy fn infs indent ln filename xs =
  let (exp,ln0,xs0) = readExpr infs Nothing [] (Left 0) ln filename xs
      (cl,ln1,xs1)  = readCl ln0 xs0
  in (fn exp cl, ln1, xs1)
  where
  readCl :: Int -> [Token] -> ([(Token,Token)], Int, [Token])
  readCl ln xs =
    case get xs of
      (_,Punct '(',xs0) -> let (mods,ln1,xs1) = readDelimiter ')' (readModifier infs) ln filename xs0
                               mods'          = transformModifier mods
                           in (mods',ln1,xs1)
      _                 -> let (mods,_,ln0,xs0) = readClauses False 
                                                              (readClause infs et False (readSub infs et) "=")
                                                              []
                                                              indent
                                                              ln
                                                              filename
                                                              xs
                           in (mods,ln0,xs0)
      
transformModifier :: [Token] -> [(Token,Token)]
transformModifier (Modifier key exp:xs) = (key,exp) : transformModifier xs
transformModifier []                    = []

readModifier :: [(String,Inf)] -> Int -> FilePath -> (Token,Int,[Token])
readModifier infs ln filename xs =
  let (key,ln0,xs0) = readSub infs Nothing ln filename xs
      (ln1,xs1)     = readEquals ln0 filename xs0
      (exp,ln2,xs2) = readExpr infs Nothing [] (Left 0) ln1 filename xs1
  in (Modifier key exp, ln2, xs2)

readExpr :: [(String,Inf)]
         -> Maybe Token
         -> [Token]
         -> Either Int Int
         -> Int
         -> FilePath
         -> [Token]
         -> (Token,Int,[Token])
readExpr infs et acc indent ln filename xs =
  case get xs of
    (_,EOF,_)       -> error' ln filename "Expected an expression"
    (_,Newline,_)   -> error' ln filename "Expected an expression. Not a newline"
    (c,_,_)         -> if case indent of
                            Left c'  -> c > c'
                            Right c' -> c == c'
                       then let (exp0,ln0,xs0) = readSub infs et ln filename xs
                                (exp1,ln1,xs1) = readExpr1 infs et exp0 ln0 filename xs0
                                (exp2,ln2,xs2) = readPostfix infs et c exp1 ln1 filename xs1
                            in case get xs2 of
                                 (_,Newline,xs3) -> let (c',_,_) = get xs3
                                                          in if c' == c
                                                             then readExpr infs et (exp2 : acc) (Right c) ln3 filename xs3
                                                             else if c' > c
                                                                  then readIf infs et c exp2 ln3 filename xs3
                                                                  else goFinish exp2 ln2 xs2
                                 _                     -> goFinish exp2 ln2 xs2
                       else error' ln filename "Bad indentation"
  where
  goFinish :: Token -> Int -> [Token] -> (Token,Int,[Token])
  goFinish exp ln xs = let ret = if null acc then exp else Seq $ reverse $ exp : acc
                       in (ret,ln,xs)

readPostfix :: [(String,Inf)]
            -> Maybe Token
            -> Int
            -> Token
            -> Int 
            -> FilePath
            -> [Token]
            -> (Token,Int,[Token])
readPostfix infs et indent exp ln filename xs =
  case get xs of
    (_,Keyword ">>",xs0)   -> readIf infs et indent exp ln filename xs0
    (_,Keyword "as",xs0)   -> readAs exp ln filename xs0
    (_,Keyword "from",xs0) -> readFrom exp ln filename xs0
    (_,Keyword "the",xs0)  -> readThe exp ln filename xs0
    (_,Punct ';',xs0)      -> let (exp2,ln1,xs1) = readExpr infs et [] (Left 0) ln filename xs0
                              in (Seq (exp2 : getSeq exp2), ln1, xs1)
    _                      -> (exp,ln,xs)

getSeq :: Token -> [Token]
getSeq (Seq ts) = ts
getSeq t        = [t]

readThe :: Token -> Int -> FilePath -> [Token] -> (Token,Int,[Token])
readThe exp ln filename xs =
  let (tp,ln0,xs0) = readType ln filename xs
  in (The tp exp, ln0, xs0)

readExprOp1 :: [(String,Token)]
            -> Maybe Token
            -> Token
            -> String
            -> Int
            -> FilePath
            -> [Token]
            -> (Token,Int,[Token])
readExprOp1 infs et exp1 s0 ln filename xs =
  let (exp2,ln0,xs0) = readSub infs et ln filename xs
  in readExprOp2 infs et exp1 s0 exp2 ln0 filename xs0

readExprOp2 :: [(String,Token)]
            -> Maybe Token
            -> Token
            -> String
            -> Token
            -> Int
            -> FilePath
            -> [Token]
            -> (Token,Int,[Token])
readExprOp2 infs et exp1 s0 exp2 ln filename xs =    
  case get xs of
    (_,EOF,_)                -> (Funcall (Op s0) (Tuple [exp1,exp2])
                                ,ln
                                ,xs)
    (_,Newline,_)            -> (Funcall (Op s0) (Tuple [exp1,exp2])
                                ,ln
                                ,xs)
    (_,Op s1,xs0)          -> case getWinner s0 s1 of
                                'l' -> readExprOp1 infs
                                                   et
                                                   (Funcall (Name s0) (concatTuple exp1 exp2))
                                                   s1
                                                   ln
                                                   filename
                                                   xs0
                                'r' -> let (exp2',ln1,xs1) = readExprOp1 infs
                                                                         et
                                                                         exp2
                                                                         s1
                                                                         ln
                                                                         filename
                                                                         xs0
                                       in (Funcall (Name s0) (concatTuple exp1 exp2')
                                          ,ln1
                                          ,xs1)
    (_,Keyword "as",xs0)   -> Funcall (Name s0) (concatTuple exp1 exp2)
    (_,Keyword ">>",xs0)   -> Funcall (Name s0) (concatTuple exp1 exp2)
    (_,Keyword "from",xs0) -> Funcall (Name s0) (concatTuple exp1 exp2)
    (_,Keyword "the",xs0)  -> Funcall (Name s0) (concatTuple exp1 exp2)
    _                      -> let (exp2',ln0,xs0) = readExpr1 infs et exp2 ln filename xs
                              in readExprOp2 infs et exp1 s0 exp2' ln0 filename xs0
  where
  getWinner :: String -> String -> Char
  getWinner s0 s1 = if s0 == s1
                    then case s0 `lookup'` infs of
                           Inf 'r' _ -> 'r'
                           Inf 'l' _ -> 'l'
                    else let Inf c0 i0 = s0 `lookup'` infs
                             Inf c1 i1 = s1 `lookup'` infs
                         in if i0 > i1 then 'l'
                            else if i1 > i0 then 'r'
                                 else if c0 == c1 then c0
                                      else error' ln filename ("Couldn't resolve winner. Please use parentheses (" ++ s0 ++ ", " ++ s1 ++ ")")
    where
    lookup' :: String -> [(String,Inf)] -> Inf
    lookup' s infs = case s `lookup` infs of
                       Just inf -> inf
                       Nothing  -> Inf 'l' 6

concatTuple :: Token -> Token -> Token
concatTuple t0 t1 = let l0 = tList t0
                        l1 = tList t1
                    in Tuple (l0 ++ l1)

tList :: Token -> [Token]
tList (Tuple tks) = tks
tList tk          = [tk]

readExpr1 :: [(String,Inf)]
          -> Maybe Token
          -> Token
          -> Int
          -> FilePath
          -> [Token]
          -> (Token,Int,[Token])
readExpr1 infs et exp1 ln filename xs =
  case get xs of
    (_,EOF,_)            -> (exp1,ln,xs)
    (_,Newline,_)        -> (exp1,ln,xs)
    (_,Op s0,xs0)        -> readExprOp1 infs et exp1 s0 ln0 filename xs0
    (_,Keyword "as",_)   -> (exp1,ln,xs)
    (_,Keyword ">>",_)   -> (exp1,ln,xs)
    (_,Keyword "from",_) -> (exp1,ln,xs)
    (_,Keyword "the",_)  -> (exp1,ln,xs)
    (_,Punct '.',xs0)    -> let (s,ln1,xs1) = readName ln0 filename xs0
                            in readExpr1 infs et (Access s exp1) ln1 filename xs1
    _                    -> let (exp2,ln0,xs0) = readSub infs DR ln filename xs
                            in case get xs0 of
                                 readExpr1 infs et (Funcall exp1 exp2) ln0 filename xs0

readFrom :: Token -> Int -> FilePath -> [Token] -> (Token,Int,[Token])
readFrom exp ln filename xs =
  let (name,ln0,xs0) = readCap ln filename xs
  in (From name exp, ln0, xs0)

readCap :: Int -> FilePath -> [Token] -> (String,[Token])
readCap ln filename xs = case get xs of
                            (_,Cap s,xs0) -> (s,xs0)
                            _             -> error' ln filename "Expected a cap name"

readIf :: [(String,Inf)]
       -> Maybe Token
       -> Int
       -> Token
       -> Int
       -> FilePath
       -> [Token]
       -> (Token,Int,[Token])
readIf infs et indent bool ln filename xs =
  let (exp,ln0,xs0)  = readExpr infs et [] (Left 0) ln filename xs
      (ln1,xs1)      = readNewline ln0 filename xs0
      (rest,ln2,xs2) = readExpr infs et [] (Right indent) ln1 filename xs1
  in (If bool exp rest,ln2,xs2)

readOpenParen :: Int -> FilePath -> [Token] -> [Token]
readOpenParen ln filename xs = case get xs of
                                 (_,Punct '(',ln0,xs0)) = xs0
                                 _                      = error' ln filename "Expected an open paren"

readRange :: [(String,Inf)]
          -> Maybe Token
          -> Int
          -> Int
          -> FilePath
          -> [Token]
          -> (Token,Int,[Token])
readRange infs et indent ln filename xs =
  let (exp,ln0,xs0)    = readExpr infs et [] (Left 0) ln filename xs
      (cl,def,ln1,xs1) = readClauses True
                                     (readClause infs et True (readSub infs et) "->")
                                     []
                                     indent
                                     ln0
                                     filename
                                     xs0
      ifs              = makeIfs exp def cl
  in (ifs,ln1,xs1)
  where
  makeIfs :: Token -> Token -> [(Token,Token)] -> Token
  makeIfs exp0 def ((key,exp1):xs) =
    case key of
      Tuple [Keyword "_",hi] -> If (Funcall (Op "<=") (Tuple [exp0,hi]))
                                    exp1
                                   (makeIfs exp0 def xs)
      Tuple [lo,Keyword "_"] -> If (Funcall (Op ">=") (Tuple [exp0,lo]))
                                    exp1
                                   (makeIfs exp0 def xs)
      Tuple [lo,hi]          -> If (Funcall (Op "&") (Tuple [Funcall (Op ">=") (Tuple [exp0,lo])
                                                           , Funcall (Op "<=") (Tuple [exp0,hi]])))
                                    exp1
                                   (makeIfs exp0 def xs)
      _                      -> error' ln filename "Bad range. Expected two element tuple"
  makeIfs _    def []              = def

readCase :: [(String,Inf)]
         -> Maybe Token
         -> Int
         -> Int
         -> FilePath
         -> [Token]
         -> (Token,Int,[Token])
readCase infs et indent ln filename xs =
  let (exp,ln0,xs0)    = readExpr infs et [] (Left 0) ln filename xs
      (cl,def,ln1,xs1) = readClauses True
                                     (readClause infs et True (readExpr infs Nothing [] (Left 0)) "->")
                                     []
                                     indent
                                     ln0
                                     filename
                                     xs0
      ifs              = makeIfs exp def cl
  in (ifs,ln1,xs1)
  where
  makeIfs :: Token -> Token -> [(Token,Token)] -> Token
  makeIfs exp0 def ((key,exp1):xs) = If (Funcall (Op "==") (Tuple [key,exp0]))
                                         exp1
                                        (makeIfs exp0 def xs)
  makeIfs _    def []              = def

readClause :: [(String,Inf)]
           -> Maybe Token
           -> Bool
           -> (Int -> FilePath -> [Token] -> (Token,Int,[Token]))
           -> Int
           -> FilePath
           -> [Token]
           -> ((Token,Token),Int,[Token])
readClause infs et allowNewline keyReader arrow ln filename xs =
  let (key,ln0,xs0) = keyReader ln filename xs
  in case get xs0 of
       (_,Newline,xs1)      -> if allowNewline
                               then let (indent,_,_)  = get xs
                                        (exp,ln2,xs2) = readExpr infs et [] (Left indent) ln0 filename xs1
                                    in ((key,exp),ln2,xs2)
                               else error' ln0 filename "Didn't expect a newline"
       (_,Keyword s,xs1)    -> if s == arrow
                               then let (exp,ln2,xs2) = readExpr infs et [] (Left 0) ln0 filename xs1
                                    in ((key,exp),ln2,xs2)
                               else error' ln1 filename "Unexpected arrow"
       _                    -> error' ln0 filename "Expected newline or arrow"

readClauses :: Bool
            -> (Int -> FilePath -> [Token] -> ((Token,Token),Int,FilePath))
            -> [(Token,Token)]
            -> Int
            -> Int
            -> FilePath
            -> [Token]
            -> ([(Token,Token)],Token,Int,[Token])
readClauses getDef fn acc indent ln filename xs =
  case get xs of
    (_,EOF,_)         -> (reverse acc, errCall, ln, xs)
    (_,Newline,xs0)   -> case get xs0 of
                           (_,EOF,_) -> (reverse acc, errCall, ln, xs)
                           (c,_,_)   -> if c > indent
                                        then let (cl,ln1,xs1) = fn ln0 filename xs0
                                             in readClauses getDef fn (cl : acc) indent ln1 filename xs1
                                        else if c == indent && getDef
                                             then let (def,ln1,xs1) = readExpr infs et [] (Right indent) ln0 filename xs0
                                                  in (reverse acc, def, ln1, xs1)
                                             else (reverse acc, errCall, ln, xs)
    _                 -> error' ln filename "Expected end of file or newline before possible clause"
  where
  errCall :: Token
  errCall = Funcall (Name "error") (String "Non-exhaustive clauses")

loopParamParam :: Token -> Token
loopParamParam (LoopParam t n _) = Param t n

loopParamArg :: Token -> Token
loopParamArg (LoopParam _ _ a) = a

readLoop :: [(String,Inf)] -> Maybe Token -> String -> Int -> Int -> FilePath -> [Token] -> (Token,Int,[Token])
readLoop infs et0 name indent ln filename xs =
  let (et1,ln0,xs0)  = if comesType ln filename xs
                       then let (t,l,x) = readType ln filename xs
                            in  (Just t,l,x)
                       else (Nothing,ln,xs)
      et'            = justOneOf et0 et1
      (ln1,xs1)      = readOpenParen ln0 filename xs0
      (lps,ln2,xs2)  = readDelimiter ')' (readLoopParam infs) ln1 filename xs1
      (ln3,xs3)      = readNewline ln2 filename xs2
      (body,ln4,xs4) = readExpr infs (Just et') [] (Left indent) ln3 filename xs3
      params         = map loopParamParam lps
      args           = Tuple $ map loopParamArg lps
  in (Let [(makeType et' params, name, Function et' params body)]
          (Funcall name args)
    , ln4
    , xs4)
  where
  justOneOf :: Maybe Token -> Maybe Token -> Token
  justOneOf (Just t) Nothing  = t
  justOneOf Nothing (Just t)  = t
  justOneOf Nothing Nothing   = error' ln filename "Loop could not deduce its' type"
  justOneOf (Just _) (Just _) = error' ln filename "Loop could already deduce its' type"
  makeType :: Token -> Token -> Token
  makeType rt params = FN rt (Tuple $ map paramType params)

readLoopParam :: [(String,Inf)] -> Int -> FilePath -> Token -> (Token,Int,[Token])
readLoopParam infs ln filename xs =
  let (tp,ln0,xs0)   = readType ln filename xs
      (name,ln1,xs1) = readName ln0 filename xs0
      (ln2,xs2)      = readEquals ln1 filename xs1
      (exp,ln3,xs3)  = readExpr infs (Just tp) [] (Left 0) ln2 filename xs2
  in (LoopParam tp name exp, ln3, xs3)

readAs :: Token -> Int -> FilePath -> [Token] -> (Token,Int,[Token])
readAs exp ln filename xs =
  let (tp,ln0,xs0) = readType ln filename xs
  in (As tp exp, ln0, xs0)

readSwitch :: [(String,Inf)]
           -> Maybe Token
           -> Int
           -> Int
           -> FilePath
           -> [Token]
           -> (Token,Int,[Token])
readSwitch infs et indent ln filename xs =
  let (exp,ln0,xs0)    = readExpr infs et [] (Left 0) ln filename xs
      (cl,def,ln1,xs1) = readClauses False
                                     (readClause infs et False (readSub infs et) "->")
                                     []
                                     indent
                                     ln0
                                     filename
                                     xs0
      cl'              = checkClauses 0 cl
  in (Switch exp cl' def, ln1, xs1)
  where
  checkClauses :: Int -> [(Token,Token)] -> [Token]
  checkClauses _ []     = []
  checkClauses n (x:xs) = case x of
                            (Int n',exp) -> if n == n' then exp : checkClauses (n + 1) xs
                                            else error' ln filename "Switch clauses must go from 0 to 1 to 2 etc"
                            _            -> error' ln filename "Switch must have only integers as keys in the clauses"

readTCase :: [(String,Inf)]
          -> Maybe Token
          -> Int
          -> Int
          -> FilePath
          -> [Token]
          -> (Token,Int,[Token])
readTCase infs et indent ln filename xs =
  let (exp,ln0,xs0)    = readExpr infs et [] (Left 0) ln filename xs
      (cl,def,ln1,xs1) = readClauses True (readClause infs et True readType "->") [] indent ln0 filename xs0
  in (TCase exp cl def, ln1, xs1)

readName :: Int -> FilePath -> [Token] -> (String,[Token])
readName ln filename xs = case get xs of
                            (_,Name s,xs0) -> (s, xs0)
                            _              -> error' ln filename "Expected a name"

comesType :: Int -> FilePath -> [Token] -> Bool
comesType ln filename xs = let (b,_,_) = comesType0 ln filename xs
                           in b

comesType0 :: Int -> FilePath -> [Token] -> (Bool,Int,[Token])
comesType0 ln filename xs =
  case comesTypeSub ln filename xs of
    (0,_,_)     -> (False,ln,xs)
    (1,ln0,xs0) -> let (ln1,xs1) = comesType1 ln0 filename xs0
                   in (True,ln1,xs1)
    (2,ln0,xs0) -> let (ln1,xs1) = comesType1s ln0 filename xs0
                   in (True,ln1,xs1)

comesTypeSub :: Int -> FilePath -> [Token] -> (Int,Int,[Token])
comesTypeSub ln filename xs =
  case get xs of
    (_,Cap "Array",xs0) -> case get xs0 of
                             (_,Int _,xs1) -> case comesTypeSub ln1 filename xs1 of
                                                (0,_,_)    -> (0,ln,xs)
                                                (_,ln2,xs2) -> (1,ln2,xs2)
                             _             -> (0,ln,xs)
    (_,Cap _,xs0)       -> (2,ln0,xs0)
    (_,Punct '[',xs0)   -> let (bool,ln1,xs1) = comesType0 ln0 filename xs0
                           in if bool
                              then case get xs1 of
                                     (_, Punct ']',xs2) -> (1,ln2,xs2)
                                     _                  -> (0,ln,xs)
                              else (0,ln,xs)
    (_,Punct '(',xs0)   -> case get xs0 of
                             (_,Newline,xs1)   -> let (indent,_,_) = get xs1
                                                  in recParenNewline indent ln1 xs1
                             (_,Punct ')',xs1) -> (1,ln1,xs1)
                             _                 -> let (bool,ln1,xs1) = comesType0 ln0 filename xs0
                                                  in if bool
                                                     then case get xs1 of
                                                            (_,Punct ',',xs2) -> recParenComma ln2 xs2
                                                            (_,Newline,xs2)   -> let (indent,_,_,_) = get xs0
                                                                                 in recParenNewline indent ln2 xs2
                                                            (_,Punct ')',xs2) -> (1,ln2,xs2)
                                                            _                 -> (0,ln,xs)
                                                     else (0,ln,xs)
    _                   -> (0,ln,xs)
  where
  recParenComma :: Int -> [Token] -> (Bool,Int,[Token])
  recParenComma ln' xs' = case comesType0 ln' filename xs' of
                            (True,ln0,xs0) -> case get xs0 of
                                                (_,Punct ')',xs1) -> (1,ln0,xs1)
                                                (_,Punct ',',xs1) -> recParenComma ln0 xs1
                                                _                 -> (0,ln,xs)
                            _              -> (0,ln,xs)
  recParenNewline :: Int -> Int -> [Token] -> (Bool,Int,[Token])
  recParenNewline indent ln' xs' =
    case get xs' of
      (_,Punct ')',xs0) -> (1,ln0,xs0)
      _                 -> let (indent',_,_,_) = get xs'
                           in if indent' == indent
                              then case comesType0 ln' filename xs' of
                                     (True,ln0,xs0) -> case get xs0 of
                                                         (_,Newline,ln1,xs1)   -> recParenNewline indent ln1 xs1
                                                         (_,Punct ')',ln1,xs1) -> (1,ln1,xs1)
                                                         _                     -> (0,ln,xs)
                                     _              -> (0,ln,xs)
                              else (0,ln,xs)

comesType1s :: Int -> FilePath -> [Token] -> (Int,[Token])
comesType1s ln filename xs = case comesTypeSub ln filename xs of
                               (0,_,_)     -> comesType1 ln filename xs
                               (_,ln0,xs0) -> comesType1 ln0 filename xs0

comesType1 :: Int -> FilePath -> [Token] -> (Int,[Token])
comesType1 ln filename xs =
  case get xs of
    (_,Op "|",xs0)  -> go ln0 xs0
    (_,Op "<-",xs0) -> go ln0 xs0
    _               -> (ln,xs)
  where
  go :: Int -> [Token] -> (Int,[Token])
  go ln' xs' = let (bool,ln0,xs0) = comesType0 ln filename xs
               in if bool then (ln0,xs0)
                          else (ln,xs)


readEndSquare :: Int -> FilePath -> [Token] -> (Int,[Token])
readEndSquare ln filename xs = case get xs of
                                 (_,Punct ']',xs0) = (ln,xs0)
                                 _                 = error' ln filename "Expected a closing square bracket"

skipAhead :: Int -> [Token] -> (Int,[Token])
skipAhead ln [] = (ln,[])
skipAhead ln xs = case head xs of
                    Newline       -> skipAhead (ln + 1) (tail xs)
                    SetCol 1      -> (ln,xs)
                    SetFileName _ -> (ln,xs)
                    _             -> skipAhead ln (tail xs)

get :: [Token] -> (Int,Token,[Token])
get (SetCol c:t:xs) = (c,t,xs)
get (SetLine ln:xs) = recNewline ln xs
  where
  recNewline :: Int -> [Token] -> (Int,Token,[Token])
  recNewline _  (SetLine ln:xs) = recNewline ln xs
  recNewline ln xs              = (0,SetLine ln,xs)
get xs = (0,EOF,xs)

readEquals :: Int -> FilePath -> [Token] -> [Token]
readEquals ln filename xs = case get xs of
                              (_,Keyword "=",xs0) -> xs0
                              _                   -> error' ln filename "Expected an equals sign"

readNewline :: Int -> FilePath -> [Token] -> [Token]
readNewline ln filename xs = case get xs of
                               (_,Newline,xs0) -> xs0
                               _               -> error' ln filename "Expected newline"

readEndBracket :: Int -> FilePath -> [Token] -> [Token]
readEndBracket ln filename xs = case get xs of
                                  (_,Punct '}',xs0) -> xs0
                                  _                 -> error' ln filename "Expected end bracket"

readDelimiter :: Char
              -> (Int -> FilePath -> [Token] -> (Token,Int,[Token]))
              -> Int
              -> FilePath
              -> [Token]
              -> ([Token],Int,[Token])
readDelimiter end fn ln filename xs =
  case get xs of
    (c,Punct p,xs0) -> if p == end
                       then ([],ln0,xs0)
                       else go c
    (_,Newline,xs0) -> case get xs0 of
                         (c,Punct p,xs1) -> if p == end
                                            then ([],ln1,xs1)
                                            else recNewline c [] ln0 xs0
                         (c,_,_)         -> recNewline c [] ln0 xs0
    (c,_,_)         -> go c
  where
  go :: Int -> ([Token], Int, [Token])
  go indent = let (tk,ln0,xs0) = fn ln filename xs
              in case get xs0 of
                   (_,Punct p,xs1) -> if p == ','
                                      then recComma [tk] ln1 xs1
                                      else if p == end
                                           then ([tk],ln1,xs1)
                                           else error' ln1 filename "Unexpected punctuation"
                   (_,Newline,xs1) -> recNewline indent [tk] ln1 xs1
                   _                   -> error' ln1 filename "Expected comma, newline or end punctuation"
  recComma :: [Token] -> Int -> [Token] -> ([Token], Int, [Token])
  recComma acc ln xs =
    let (tk,ln0,xs0) = fn ln filename xs
    in case get xs0 of
         (_,Punct p,xs1) -> if p == ','
                            then recComma (tk : acc) ln0 xs1
                            else if p == end
                                 then (reverse $ tk : acc, ln0, xs1)
                                 else error' ln1 filename "Unexpected punctuation"
         _               -> error' ln0 filename "Expected end punctuation or comma"
  recNewline :: Int -> [Token] -> Int -> [Token] -> ([Token], Int, [Token])
  recNewline indent acc ln xs =
    case get xs of
      (c,Punct p,xs0) -> if p == end
                         then (reverse acc, ln0, xs0)
                         else goNewline c
      (c,_,_)         -> goNewline c
    where
    goNewline :: Int -> ([Token],Int,[Token])
    goNewline indent' = if indent' == indent
                        then let (tk,ln0,xs0) = fn ln filename xs
                             in case get xs0 of
                                  (_,Punct p,xs1) -> if p == end
                                                     then (reverse $ tk : acc, ln0, xs1)
                                                     else error' ln0 filename "Expected newline or end punctuation"
                                  (_,Newline,xs1) -> recNewline indent (tk : acc) ln1 xs1
                        else error' ln1 filename "Unexpected indentation"

readType :: Int -> FilePath -> [Token] -> (Token, Int, [Token])
readType ln filename xs = case readTypeSub ln filename xs of
                            (NM s _, ln0, xs0) -> readType1s s ln0 filename xs0
                            (t, ln0, xs0)          -> readType1 t ln0 filename xs0

readTypeSub :: Int -> FilePath -> [Token] -> (Token,Int,[Token])
readTypeSub ln filename xs =
  case get xs of
    (_,Cap "Array",xs0) -> case get xs0 of
                             (_,Int n,xs1) -> let (tp,ln2,xs2) = readTypeSub ln1 filename xs1
                                              in (AR n tp,ln2,xs2)
                             _             -> error' ln0 filename "Expected integer after Array"
    (_,Punct '[',xs0)   -> let (tp,ln1,xs1) = readType ln0 filename xs0
                               (ln2,xs2)    = readEndSquare ln1 filename xs1
                           in (AR 1 tp, ln2, xs2)
    (_,Punct '{',xs0)   -> let (tp,ln1,xs1) = readType ln0 filename xs0
                               (ln2,xs2)    = readEndBracket ln1 filename xs1
                           in (NM "List" [tp], ln2, xs2)
    (_,Punct '(',xs0)   -> let (tps,ln1,xs1) = readDelimiter ')' readType ln0 filename xs0
                           in (Tuple tps, ln1, xs1)
    (_,Cap name,xs0)    -> (NM name (Tuple []), ln0, xs0)
    _                   -> error' ln filename "Expected a type"

readType1s :: String -> Int -> FilePath -> [Token] -> (Token,Int,[Token])
readType1s s ln filename xs =
  if comesType ln filename xs
  then let (tp,ln0,xs0) = readTypeSub ln filename xs
       in readType1 (NM s tp)
                     ln0
                     filename
                     xs0
  else readType1 (NM s (Tuple []))
                  ln
                  filename
                  xs

readType1 :: Token -> Int -> FilePath -> [Token] -> (Token, Int, [Token])
readType1 t0 ln filename xs =
  case get xs of
    (_,Op "|",xs0)       -> let (t1,ln1,xs1) = readType ln filename xs0
                            in (UN (t0 : unionToList t1), ln1, xs1)
    (_,Keyword "<-",xs0) -> let (t1,ln1,xs1) = readType ln filename xs0
                            in (FN t0 t1, ln1, xs1)
    _                    -> (t0,ln,xs)

unionToList :: Token -> [Token]
unionToList (UN xs) = xs
unionToList x          = [x]

 -- Error

error' :: Int -> FilePath -> String -> a
error' ln filename message = error ("Parse error in file " ++ filename ++
                                  " on line " ++ show ln ++ "\n" ++ message)
