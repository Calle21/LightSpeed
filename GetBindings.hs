module GetBindings (skipAhead
                   ,getBindings
                   ,get
                   ,typeKeyword
                   ,readEquals
                   ,readNewline
                   ,comesType
                   ,readCap
                   ,readType
                   ,readDelimiter
                   ,error') where

import Share (Token(..), Inf(Inf))

getBindings :: [(String,Inf)] -> [Token] -> [(Token, String, Token)]
getBindings infs xs = case xs of
                       (SetFileName s:xs') -> recTop infs 1 s xs'
                       _                   -> error "Expected filename setter for getBindings"

transformLet :: ([Token],[String],[Token]) -> [(Token,String,Token)]
transformLet (x:xs,y:ys,z:zs) = (x,y,z) : transformLet (xs,ys,zs)
transformLet []               = []

typeKeyword :: String -> Bool
typeKeyword s = s `elem` ["struct","union","type","synonym"]

comesTypeDeclaration :: Int -> FilePath -> [Token] -> Bool
comesTypeDeclaration ln filename xs = case get ln filename xs of
                                        (1,Keyword s,_,_) -> typeKeyword s
                                        _                 -> False

recTop :: [(String,Inf)] -> Int -> FilePath -> [Token] -> [(Token,String,Token)]
recTop infs ln filename xs =
  case xs of
    (SetFileName s:xs') -> recTop infs 1 s xs'
    (SetCol 1:xs')      -> if comesTypeDeclaration ln filename xs
                           then let (ln',xs') = skipAhead ln xs'
                                in recTop infs ln' filename xs'
                           else let (types,names,vals,ln',xs') = readBinding infs ln filename xs
                                in transformLet (types,names,vals) ++ recTop infs ln' filename xs'
    (Newline:xs')       -> recTop infs ln' filename xs'
    []                  -> []
    _                   -> error' ln filename "Top-level expressions must start on column 1"

readBinding :: [(String, Inf)] -> Int -> FilePath -> [Token] -> ([Token],[String],[Token],Int,[Token])
readBinding infs ln filename xs =
  let (bind,ln0,xs0) = if comesType ln filename xs
                       then let (tnp,ln0,xs0) = readTNP ln filename xs
                            in (Right tnp,ln0,xs0)
                       else let (ln1,xs1) = readOpenParen ln0 filename xs0
                                (tns,ln2,xs2) = readDelimiter ')' readTypeAndName ln0 filename xs0
                            in (Left tns,ln2,xs2)
      (exp,ln1,xs1)  = case get ln0 filename xs0 of
                         (_,Keyword "=",ln1,xs1) -> readExpr infs (Just $ getReturnType bind) [] (Left 0) ln1 filename xs1
                         (_,Newline,ln1,xs1)     -> let (indent,_,_,_) = get ln filename xs
                                                    in readExpr infs (Just $ getReturnType bind) [] (Left indent) ln1 filename xs1
                         _                       -> error' ln1 filename "Expected equals sign or newline"
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

readTNP :: Int -> FilePath -> [Token] -> (Token,Int,[Token])
readTNP ln filename xs =
  let (rt,ln0,xs0)        = readType ln filename xs
      (nm,params,ln1,xs1) = readNameAndParams ln0 xs0
  in (TNP rt nm params, ln1, xs1)
  where
  readNameAndParams :: Int -> [Token] -> (String,[Token],Int,[Token])
  readNameAndParams ln xs =
    case get ln filename xs of
      (_,Name s,ln0,xs0)    -> let (params,ln1,xs1) = readParams ln0 filename xs0
                               in (s,params,ln1,xs1)
      (_,Punct '(',ln0,xs0) -> let (param1,ln1,xs1) = readParam ln0 filename xs0
                                   (ln2,xs2)        = readCloseParen ln1 filename xs1
                                   (name,ln3,xs3)   = readOp ln2 filename xs2
                                   (ln4,xs4)        = readOpenParen ln3 filename xs3
                                   (param2,ln5,xs5) = readParam ln4 filename xs4
                                   (ln6,xs6)        = readCloseParen ln5 filename xs5
                               in (name,[param1,param2],ln6,xs6)
      _                     -> error' ln filename "Expected name or open paren"

readCloseParen :: Int -> FilePath -> [Token] -> (Int,[Token])
readCloseParen ln filename xs = case get ln filename xs of
                                  (_,Punct ')',ln0,xs0) -> (ln0,xs0)
                                  (_,_,ln0,_)           -> error' ln0 filename "Expected a close paren"

readOp :: Int -> FilePath -> [Token] -> (String,Int,[Token])
readOp ln filename xs = case get ln filename xs of
                          (_,Op s,ln0,xs0) -> (s,ln0,xs0)
                          _                -> error' ln filename "Expected an operator name"

readParams :: Int -> FilePath -> [Token] -> ([Token],Int,[Token])
readParams ln filename xs =
  case get ln filename xs of
    (_,Keyword "=",_,_)   = ([],ln,xs)
    (_,Newline,_,_)       = ([],ln,xs)
    (_,Punct '(',ln0,xs0) = let (params,ln1,xs1) = readDelimiter ')' readParam ln0 filename xs0
                            in if null params then error' ln filename "Empty parameter list. Omit it instead"
                               else (params, ln1, xs1)
    _                     = error' ln filename "Expected equals sign, newline or parameter list"

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
      (body,ln3,xs3)   = case get ln2 filename xs2 of
                           (_,Keyword "=",ln3,xs3) -> readExpr infs (Just rt) [] (Left 0) ln3 filename xs3
                           (_,Newline,ln3,xs3)     -> readExpr infs (Just rt) [] (Left indent) ln3 filename xs3
                           _                       -> error' ln2 filename "Expected equals sign or newline"
  in (Function rt params body, ln3, xs3)

readSub :: [(String,Inf)] -> Maybe Token -> Int -> FilePath -> [Token] -> (Token,Int,[Token])
readSub infs et ln filename xs =
  if comesType ln filename xs
  then let (tps,nms,vals,ln0,xs0) = readBinding infs ln filename xs
           (ln1,xs1)              = readNewline ln0 filename xs0
           (indent,_,_,_)         = get ln filename xs
           (rest,ln2,xs2)         = readExpr infs et [] (Right indent) ln1 filename xs1
       in (Let (transformLet (tps,nms,vals)) rest, ln2, xs2)
  else let (c,t,ln0,xs0) = get ln filename xs
       in case t of
            Keyword "lambda" -> readLambda infs c ln0 filename xs0
            Keyword "case"   -> readCase infs et c ln0 filename xs0
            Keyword "tcase"  -> readTCase infs et c ln0 filename xs0
            Keyword "switch" -> readSwitch infs et c ln0 filename xs0
            Keyword "range"  -> readRange infs et c ln0 filename xs0
            Keyword "modify" -> readModify infs c ln0 filename xs0
            Access _ _       -> (t,ln0,xs0)
            Char _           -> (t,ln0,xs0)
            Float _          -> (t,ln0,xs0)
            Int _            -> (t,ln0,xs0)
            Keyword "..."    -> (t,ln0,xs0)
            Keyword "_"      -> (t,ln0,xs0)
            Name _           -> (t,ln0,xs0)
            String _         -> (t,ln0,xs0)
            Punct '('        -> let (exps,ln1,xs1) = readDelimiter ')'
                                                                   (readExpr infs Nothing [] (Left 0))
                                                                   ln0
                                                                   filename
                                                                   xs0
                                in (Tuple exps, ln1, xs1)
            Punct '['        -> let (exps,ln1,xs1) = readDelimiter ']'
                                                                   (readExpr infs Nothing [] (Left 0))
                                                                   ln0
                                                                   filename
                                                                   xs0
                                in (Array [length exps] exps, ln1, xs1)
            Punct '{'        -> let (exps,ln1,xs1) = readDelimiter '}'
                                                                   (readExpr infs Nothing [] (Left 0))
                                                                   ln0
                                                                   filename
                                                                   xs0
                                in (makeList exps, ln1, xs1)
            Loop name        -> readLoop infs et name c ln0 filename xs0
            _                -> error' ln filename "Expected sub-expression"

makeList :: [Token] -> Token
makeList xs = undefined

readModify :: [(String,Inf)] -> Int -> Int -> FilePath -> [Token] -> (Token,Int,[Token])
readModify infs indent ln filename xs =
  let (exp,ln0,xs0) = readExpr infs Nothing [] (Left 0) ln filename xs
      (cl,ln1,xs1)  = readCl ln0 xs0
  in (Modify exp cl, ln1, xs1)
  where
  readCl :: Int -> [Token] -> ([(Token,Token)], Int, [Token])
  readCl ln xs =
    case get ln filename xs of
      (_,Punct '(',ln0,xs0) -> let (mods,ln1,xs1) = readDelimiter ')' (readModifier infs) ln0 filename xs0
                                   mods'          = transformModifier mods
                               in (mods',ln1,xs1)
      _                     -> let (mods,_,ln0,xs0) = readClauses False 
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
  case get ln filename xs of
    (_,EOF,_,_)       -> error' ln filename "Expected an expression"
    (_,Newline,_,_)   -> error' ln filename "Expected an expression. Not a newline"
    (c,_,_,_)         -> if case indent of
                              Left c'  -> c > c'
                              Right c' -> c == c'
                         then let (exp0,ln0,xs0) = readSub infs et ln filename xs
                                  (exp1,ln1,xs1) = readExpr1 infs et exp0 ln0 filename xs0
                                  (exp2,ln2,xs2) = readPostfixKeyword infs et c exp1 ln1 filename xs1
                              in case get ln2 filename xs2 of
                                   (_,Newline,ln3,xs3) -> let (c',_,_,_) = get ln3 filename xs3
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

readPostfixKeyword :: [(String,Inf)]
                   -> Maybe Token
                   -> Int
                   -> Token
                   -> Int 
                   -> FilePath
                   -> [Token]
                   -> (Token,Int,[Token])
readPostfixKeyword infs et indent exp ln filename xs =
  case get ln filename xs of
    (_,Keyword ">>",ln0,xs0)   -> readIf infs et indent exp ln0 filename xs0
    (_,Keyword "as",ln0,xs0)   -> readAs exp ln0 filename xs0
    (_,Keyword "from",ln0,xs0) -> readFrom exp ln0 filename xs0
    (_,Keyword "the",ln0,xs0)  -> readThe exp ln0 filename xs0
    _                          -> (exp,ln,xs)

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
  case get ln filename xs of
    (_,EOF,_,_)                -> (Funcall (Op s0) (Tuple [exp1,exp2])
                                  ,ln
                                  ,xs)
    (_,Newline,_,_)            -> (Funcall (Op s0) (Tuple [exp1,exp2])
                                  ,ln
                                  ,xs)
    (_,Op s1,ln0,xs0)          -> case getWinner s0 s1 of
                                    'l' -> readExprOp1 infs
                                                       et
                                                       (Funcall (Op s0) (Tuple [exp1,exp2]))
                                                       s1
                                                       ln0
                                                       filename
                                                       xs0
                                    'r' -> let (exp2',ln1,xs1) = readExprOp1 infs
                                                                             et
                                                                             exp2
                                                                             s1
                                                                             ln0
                                                                             filename
                                                                             xs0
                                           in (Funcall (Op s0) (Tuple [exp1,exp2'])
                                              ,ln1
                                              ,xs1)
    (_,Keyword "as",ln0,xs0)   -> Funcall (Op s0) (Tuple [exp1,exp2])
    (_,Keyword ">>",ln0,xs0)   -> Funcall (Op s0) (Tuple [exp1,exp2])
    (_,Keyword "from",xs0,ln0) -> Funcall (Op s0) (Tuple [exp1,exp2])
    (_,Keyword "the",xs0,ln0)  -> Funcall (Op s0) (Tuple [exp1,exp2])
    _                          -> let (exp2',ln0,xs0) = readExpr1 infs et exp2 ln filename xs
                                  in readExprOp2 infs et exp1 s0 exp2' ln0 filename xs0
  where
  getWinner :: String -> String -> Char
  getWinner s0 s1 = if s0 == s1
                    then case s0 `lookup'` infs of
                           Inf 'r' _ -> 'r'
                           Inf 'l' _ -> 'l'
                    else let Inf _ s0' = s0 `lookup'` infs
                             Inf _ s1' = s1 `lookup'` infs
                         in if s0' > s1' then 'l'
                            else if s1' > s0' then 'r'
                                 else error' ln filename ("These operators have the same precedence level. Please use parentheses: "
                                                            ++ s0 ++ " " ++ s1)
    where
    lookup' :: String -> [(String,Inf)] -> Inf
    lookup' s infs = case s `lookup` infs of
                       Just inf -> inf
                       Nothing  -> error' ln filename ("No infix declaration for op " ++ s)

readExpr1 :: [(String,Inf)]
          -> Maybe Token
          -> Token
          -> Int
          -> FilePath
          -> [Token]
          -> (Token,Int,[Token])
readExpr1 infs et exp1 ln filename xs =
  case get ln filename xs of
    (_,EOF,_,_)            -> (exp1,ln,xs)
    (_,Newline,_,_)        -> (exp1,ln,xs)
    (_,Op s0,ln0,xs0)      -> readExprOp1 infs et exp1 s0 ln0 filename xs0
    (_,Keyword "as",_,_)   -> (exp1,ln,xs)
    (_,Keyword ">>",_,_)   -> (exp1,ln,xs)
    (_,Keyword "from",_,_) -> (exp1,ln,xs)
    (_,Keyword "the",_,_)  -> (exp1,ln,xs)
    (_,Punct '.',ln0,xs0)  -> let (s,ln1,xs1) = readName ln0 filename xs0
                              in readExpr1 infs et (Access s exp1) ln1 filename xs1
    _                      -> let (exp2,ln0,xs0) = readSub infs Nothing ln filename xs
                              in readExpr1 infs et (Funcall exp1 exp2) ln0 filename xs0

readFrom :: Token -> Int -> FilePath -> [Token] -> (Token,Int,[Token])
readFrom exp ln filename xs =
  let (name,ln0,xs0) = readCap ln filename xs
  in (From name exp, ln0, xs0)

readCap :: Int -> FilePath -> [Token] -> (String,Int,[Token])
readCap ln filename xs = case get ln filename xs of
                            (_,Cap s,ln0,xs0) -> (s, ln0, xs0)
                            _                 -> error' ln filename "Expected a cap name"

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

readOpenParen :: Int -> FilePath -> [Token] -> (Int,[Token])
readOpenParen ln filename xs = case get ln filename xs of
                                 (_,Punct '(',ln0,xs0)) = (ln0,xs0)
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
  in case get ln0 filename xs0 of
       (_,Newline,ln1,xs1)      -> if allowNewline
                                   then let (indent,_,_,_)  = get ln filename xs
                                            (exp,ln2,xs2) = readExpr infs et [] (Left indent) ln1 filename xs1
                                        in ((key,exp),ln2,xs2)
                                   else error' ln0 filename "Didn't expect a newline"
       (_,Keyword s,ln1,xs1)    -> if s == arrow
                                   then let (exp,ln2,xs2) = readExpr infs et [] (Left 0) ln1 filename xs1
                                        in ((key,exp),ln2,xs2)
                                   else error' ln1 filename "Unexpected arrow"
       _                        -> error' ln0 filename "Expected newline or arrow"

readClauses :: Bool
            -> (Int -> FilePath -> [Token] -> ((Token,Token),Int,FilePath))
            -> [(Token,Token)]
            -> Int
            -> Int
            -> FilePath
            -> [Token]
            -> ([(Token,Token)],Token,Int,[Token])
readClauses getDef fn acc indent ln filename xs =
  case get ln filename xs of
    (_,EOF,_,_)           -> (reverse acc, errCall, ln, xs)
    (_,Newline,ln0,xs0)   -> case get ln0 filename xs0 of
                               (_,EOF,_,_) -> (reverse acc, errCall, ln, xs)
                               (c,_,_,_)   -> if c > indent
                                              then let (cl,ln1,xs1) = fn ln0 filename xs0
                                                   in readClauses getDef fn (cl : acc) indent ln1 filename xs1
                                              else if c == indent && getDef
                                                   then let (def,ln1,xs1) = readExpr infs et [] (Right indent) ln0 filename xs0
                                                        in (reverse acc, def, ln1, xs1)
                                                   else (reverse acc, errCall, ln, xs)
    _                     -> error' ln filename "Expected end of file or newline before possible clause"
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

readName :: Int -> FilePath -> [Token] -> (String,Int,[Token])
readName ln filename xs = case get ln filename xs of
                            (_,Name s,ln0,xs0) -> (s, ln0, xs0)
                            _                  -> error' ln filename "Expected a name"

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
  case get ln filename xs of
    (_,Cap "Array",ln0,xs0) -> case get ln0 filename xs0 of
                                 (_,Int _,ln1,xs1) -> case comesTypeSub ln1 filename xs1 of
                                                        (0,_,_)    -> (0,ln,xs)
                                                        (_,ln2,xs2) -> (1,ln2,xs2)
                                 _                 -> (0,ln,xs)
    (_,Cap _,ln0,xs0)       -> (2,ln0,xs0)
    (_,Punct '[',ln0,xs0)   -> let (bool,ln1,xs1) = comesType0 ln0 filename xs0
                               in if bool
                                  then case get ln1 filename xs1 of
                                         (_, Punct ']',ln2,xs2) -> (1,ln2,xs2)
                                         _                      -> (0,ln,xs)
                                  else (0,ln,xs)
    (_,Punct '(',ln0,xs0)   -> case get ln0 filename xs0 of
                                 (_,Newline,ln1,xs1)   -> let (indent,_,_,_) = get ln1 filename xs1
                                                          in recParenNewline indent ln1 xs1
                                 (_,Punct ')',ln1,xs1) -> (1,ln1,xs1)
                                 _                     -> let (bool,ln1,xs1) = comesType0 ln0 filename xs0
                                                          in if bool
                                                             then case get ln1 filename xs1 of
                                                                    (_,Punct ',',ln2,xs2) -> recParenComma ln2 xs2
                                                                    (_,Newline,ln2,xs2)   -> let (indent,_,_,_) = get ln0 filename xs0
                                                                                             in recParenNewline indent ln2 xs2
                                                                    (_,Punct ')',ln2,xs2) -> (1,ln2,xs2)
                                                                    _                     -> (0,ln,xs)
                                                             else (0,ln,xs)
    _                       -> (0,ln,xs)
  where
  recParenComma :: Int -> [Token] -> (Bool,Int,[Token])
  recParenComma ln' xs' = case comesType0 ln' filename xs' of
                            (True,ln0,xs0) -> case get ln0 filename xs0 of
                                                (_,Punct ')',ln1,xs1) -> (1,ln1,xs1)
                                                (_,Punct ',',ln1,xs1) -> recParenComma ln1 xs1
                                                _                     -> (0,ln,xs)
                            _              -> (0,ln,xs)
  recParenNewline :: Int -> Int -> [Token] -> (Bool,Int,[Token])
  recParenNewline indent ln' xs' =
    case get ln' filename xs' of
      (_,Punct ')',ln0,xs0) -> (1,ln0,xs0)
      _                     -> let (indent',_,_,_) = get ln' filename xs'
                               in if indent' == indent
                                  then case comesType0 ln' filename xs' of
                                         (True,ln0,xs0) -> case get ln0 filename xs0 of
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
  case get ln filename xs of
    (_,Op "|",ln0,xs0)  -> go ln0 xs0
    (_,Op "<-",ln0,xs0) -> go ln0 xs0
    _                   -> (ln,xs)
  where
  go :: Int -> [Token] -> (Int,[Token])
  go ln' xs' = let (bool,ln0,xs0) = comesType0 ln filename xs
               in if bool then (ln0,xs0)
                          else (ln,xs)


readEndSquare :: Int -> FilePath -> [Token] -> (Int,[Token])
readEndSquare ln filename xs = case get ln filename xs of
                                 (_,Punct ']',ln0,xs0) = (ln0,xs0)
                                 _                     = error' ln filename "Expected a closing square bracket"

skipAhead :: Int -> [Token] -> (Int,[Token])
skipAhead ln [] = (ln,[])
skipAhead ln xs = case head xs of
                    Newline       -> skipAhead (ln + 1) (tail xs)
                    SetCol 1      -> (ln,xs)
                    SetFileName _ -> (ln,xs)
                    _             -> skipAhead ln (tail xs)

get :: Int -> FilePath -> [Token] -> (Int,Token,Int,[Token])
get ln filename (SetCol c:t:xs) = case t of
                                    Keyword "\\" -> case xs of
                                                      (Newline:xs0) -> get (ln + 1) filename xs0
                                                      _             -> error' ln filename "Expected newline after backslash"
                                    _            -> (c,t,xs,ln)
get ln _        (Newline:xs)    = recNewline (ln + 1) xs
  where
  recNewline :: Int -> [Token] -> (Int,Token,Int,[Token])
  recNewline ln (Newline:xs) = recNewline (ln + 1) xs
  recNewline ln xs           = (0,Newline,ln,xs)
get ln _        xs              = (0,EOF,ln,xs)

readEquals :: Int -> FilePath -> [Token] -> (Int,[Token])
readEquals ln filename xs = case get ln filename xs of
                              (_,Keyword "=",ln0,xs0) -> (ln0,xs0)
                              _                       -> error' ln filename "Expected an equals sign"

readNewline :: Int -> FilePath -> [Token] -> (Int,[Token])
readNewline ln filename xs = case get ln filename xs of
                               (_,Newline,ln0,xs0) -> (ln0,xs0)
                               _                   -> error' ln filename "Expected newline"

readDelimiter :: Char
              -> (Int -> FilePath -> [Token] -> (Token,Int,[Token]))
              -> Int
              -> FilePath
              -> [Token]
              -> ([Token],Int,[Token])
readDelimiter end fn ln filename xs =
  case get ln filename xs of
    (c,Punct p,ln0,xs0) -> if p == end
                           then ([],ln0,xs0)
                           else go c
    (_,Newline,ln0,xs0) -> case get ln0 filename xs0 of
                             (c,Punct p,ln1,xs1) -> if p == end
                                                    then ([],ln1,xs1)
                                                    else recNewline c [] ln0 xs0
                             (c,_,_,_)           -> recNewline c [] ln0 xs0
    (c,_,_,_)           -> go c
  where
  go :: Int -> ([Token], Int, [Token])
  go indent = let (tk,ln0,xs0) = fn ln filename xs
              in case get ln0 filename xs0 of
                   (_,Punct p,ln1,xs1) -> if p == ','
                                             then recComma [tk] ln1 xs1
                                             else if p == end
                                                  then ([tk],ln1,xs1)
                                                  else error' ln1 filename "Unexpected punctuation"
                   (_,Newline,ln1,xs1) -> recNewline indent [tk] ln1 xs1
                   _                   -> error' ln1 filename "Expected comma, newline or end punctuation"
  recComma :: [Token] -> Int -> [Token] -> ([Token], Int, [Token])
  recComma acc ln xs =
    let (tk,ln0,xs0) = fn ln filename xs
    in case get ln0 filename xs0 of
         (_,Punct p,ln1,xs1) -> if p == ','
                                then recComma (tk : acc) ln1 xs1
                                else if p == end
                                     then (reverse $ tk : acc, ln1, xs1)
                                     else error' ln1 filename "Unexpected punctuation"
         _                   -> error' ln0 filename "Expected end punctuation or comma"
  recNewline :: Int -> [Token] -> Int -> [Token] -> ([Token], Int, [Token])
  recNewline indent acc ln xs =
    case get ln filename xs of
      (c,Punct p,ln0,xs0) -> if p == end
                             then (reverse acc, ln0, xs0)
                             else goNewline c
      (c,_,_,_)           -> goNewline c
    where
    goNewline :: Int -> ([Token],Int,[Token])
    goNewline indent' = if indent' == indent
                        then let (tk,ln0,xs0) = fn ln filename xs
                             in case get ln0 filename xs0 of
                                  (_,Punct p,ln1,xs1) -> if p == end
                                                         then (reverse $ tk : acc, ln1, xs1)
                                                         else error' ln1 filename "Expected newline or end punctuation"
                                  (_,Newline,ln1,xs1) -> recNewline indent (tk : acc) ln1 xs1
                        else error' ln1 filename "Unexpected indentation"

readType :: Int -> FilePath -> [Token] -> (Token, Int, [Token])
readType ln filename xs = case readTypeSub ln filename xs of
                            (LU s _, ln0, xs0) -> readType1s s ln0 filename xs0
                            (t, ln0, xs0)          -> readType1 t ln0 filename xs0

readTypeSub :: Int -> FilePath -> [Token] -> (Token,Int,[Token])
readTypeSub ln filename xs =
  case get ln filename xs of
    (_,Cap "Array",ln0,xs0) -> case get ln0 filename xs0 of
                                 (_,Int n,ln1,xs1) -> let (tp,ln2,xs2) = readTypeSub ln1 filename xs1
                                                      in (AR n tp,ln2,xs2)
                                 _                 -> error' ln0 filename "Expected integer after Array"
    (_,Punct '[',ln0,xs0)   -> let (tp,ln1,xs1) = readType ln0 filename xs0
                                   (ln2,xs2)    = readEndSquare ln1 filename xs1
                               in (AR 1 tp, ln2, xs2)
    (_,Punct '(',ln0,xs0)   -> let (tps,ln1,xs1) = readDelimiter ')' readType ln0 filename xs0
                               in (Tuple tps, ln1, xs1)
    (_,Cap name,ln0,xs0)    -> (LU name (Tuple []), ln0, xs0)
    _                       -> error' ln filename "Expected a type"

readType1s :: String -> Int -> FilePath -> [Token] -> (Token,Int,[Token])
readType1s s ln filename xs =
  if comesType ln filename xs
  then let (tp,ln0,xs0) = readTypeSub ln filename xs
       in readType1 (LU s tp)
                     ln0
                     filename
                     xs0
  else readType1 (LU s (Tuple []))
                  ln
                  filename
                  xs

readType1 :: Token -> Int -> FilePath -> [Token] -> (Token, Int, [Token])
readType1 t0 ln filename xs =
  case get ln filename xs of
    (_,Op "|",ln0,xs0)       -> let (t1,ln1,xs1) = readType ln0 filename xs0
                                in (UN (t0 : unionToList t1), ln1, xs1)
    (_,Keyword "<-",ln0,xs0) -> let (t1,ln1,xs1) = readType ln0 filename xs0
                                in (FN t0 t1, ln1, xs1)
    _                        -> (t0,ln,xs)

unionToList :: Token -> [Token]
unionToList (UN xs) = xs
unionToList x          = [x]

 -- Error

error' :: Int -> FilePath -> String -> a
error' ln filename message = error ("Parse error in file " ++ filename ++
                                  " on line " ++ show ln ++ "\n" ++ message)
