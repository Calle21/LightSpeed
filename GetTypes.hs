module GetTypes (getTypes) where

import Share (Token(..))
import GetBindings (skipAhead
                   ,get
                   ,comesType
                   ,typeKeyword
                   ,readEquals
                   ,readNewline
                   ,readType
                   ,readCap
                   ,readDelimiter
                   ,error')

type TypeReader = Int -> FilePath -> [Token] -> (String,Token,Int,[Token])

getTypes :: [Token] -> [(String, Token)]
getTypes (x:xs) = case x of
                    SetFileName s -> recTop 1 s xs
                    _             -> error "Expected a filename setter for getTypes"

recTop :: Int -> FilePath -> [Token] -> [(String, Token)]
recTop ln filename xs =
  case get ln filename xs of
    (1,Keyword s,ln0,xs0)   -> if typeKeyword s
                               then let fn = case s of
                                               "type"    -> readType'
                                               "struct"  -> readStruct
                                               "union"   -> readUnion
                                               "synonym" -> readSynonym
                                        (nm,tp,ln1,xs1) = fn ln0 filename xs0
                                    in (nm,tp) : recTop ln1 filename xs1
                               else goSkip ln0 xs0
    (_,Newline,ln0,xs0)     -> recTop ln0 filename xs0
    (_,SetFileName s,_,xs0) -> recTop 1 s xs0
    (_,EOF,_,_)             -> []
    (_,_,ln0,xs0)           -> goSkip ln0 xs0
  where
  goSkip :: Int -> [Token] -> [(String,Token)]
  goSkip ln xs = let (ln0,xs0) = skipAhead ln xs
                 in recTop ln0 filename xs0

readSynonym :: TypeReader
readSynonym ln filename xs = let (nm,ln0,xs0) = readCap ln filename xs
                                 (ln1,xs1)    = readEquals ln0 filename xs0
                                 (tp,ln2,xs2) = readType ln1 filename xs1
                             in (nm,SY tp,ln2,xs2)

readVars :: Int -> FilePath -> [Token] -> ([String],Int,[Token])
readVars ln filename xs =
  case get ln filename xs of
    (_,Punct '(',ln0,xs0) -> let (tks,ln1,xs1) = readDelimiter ')' readAllCap' ln0 filename xs0
                                 ss            = map allCapString tks
                             in (ss,ln1,xs1)
    (_,AllCap s,ln0,xs0)  -> ([s],ln0,xs0)
    _                     -> ([],ln,xs)

allCapString :: Token -> String
allCapString (AllCap s) = s

readAllCap' :: Int -> FilePath -> [Token] -> (Token,Int,[Token])
readAllCap' ln filename xs = case get ln filename xs of
                               (_,AllCap s,ln0,xs0) -> (AllCap s,ln0,xs0)
                               _                    -> error' ln filename "Expected an all cap"

readUnion :: TypeReader
readUnion ln filename xs = let (nm,ln0,xs0)   = readCap ln filename xs
                               (vars,ln1,xs1) = readVars ln0 filename xs0
                               (ln2,xs2)      = readEquals ln1 filename xs1
                               (mems,ln3,xs3) = readMembers [] ln2 xs2
                           in if length mems < 2 then error' ln filename "Should be at least two members in union"
                              else (nm, mkType vars $ Union mems, xs3)
  where
  readMembers :: [Token] -> Int -> [Token] -> ([Token],Int,[Token])
  readMembers acc ln xs =
    let (tp,ln0,xs0) = readType ln filename xs
    in case tp of
         UN _ -> error' ln filename "Unions cannot contain unions"
         _    -> case get ln0 filename xs0 of
                   (_,Punct ',',ln1,xs1) -> readMembers (tp : acc) ln1 xs1
                   _                     -> (tp : acc,ln0,xs0)

mkType :: [String] -> Token -> Token
mkType [] tp = tp
mkType xs tp = PR xs tp

readType' :: TypeReader
readType' ln filename xs = let (nm,ln0,xs0) = readCap ln filename xs
                               (ln1,xs1)    = readEquals ln0 filename xs0
                               (tp,ln2,xs2) = readType ln1 filename xs1
                           in (nm,TP tp,ln2,xs2)

readStruct :: TypeReader
readStruct ln filename xs = let (name,ln0,xs0)   = readCap ln filename xs
                                (vars,ln1,xs1)   = readVars ln0 filename xs0
                                (ln2,xs2)        = readNewline ln1 filename xs1
                                (fields,ln3,xs3) = readFields [] ln2 filename xs2
                            in (name, mkType vars $ ST fields, ln3, xs3)

readFields :: [(Token,String)] -> Int -> FilePath -> [Token] -> ([(Token,String)], Int, [Token])
readFields acc ln filename xs =
  if comesType ln filename xs
  then let (field,ln0,xs0) = readField ln filename xs
       in case get ln0 filename xs0 of
            (_,EOF,_,_)         -> (reverse $ field : acc,ln0,xs0)
            (_,Newline,ln1,xs1) -> readFields (field : acc) ln1 filename xs1
            _                   -> error' ln0 filename "Expected newline or end of file"
  else (reverse acc,ln,xs)

readField :: Int -> FilePath -> [Token] -> ((Token,String),Int,[Token])
readField ln filename xs = let (tp,ln0,xs0) = readType ln filename xs
                               (nm,ln1,xs1) = readName ln0 filename xs0
                           in ((tp,nm),ln1,xs1)
