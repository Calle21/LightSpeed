module Type where

import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map
import qualified Data.Set.Strict as Set
import Ubi
import Util(String')

 -- Binding

 -- BindPat

 -- Code

 -- CompFN

type CompFN = Setup -> FileWPath -> (File,Setup)

 -- Directory

type Directory = [DirFile]

 -- DirFile

data DirFile = Folder (FilePath, Directory)
             | File   FileWPath
             deriving (Read, Show)

countFiles :: Directory -> Int
countFiles (x:xs) = case x of
                      File   _       -> 1 + countFiles xs
                      Folder (_,dir) -> countFiles dir + countFiles xs
countFiles []     = 0

mapDir :: Setup -> CompFN -> Directory -> Directory
mapDir setup fn (x:xs) = case x of
                           File   fw      -> let (file,setup') = fn setup fw
                                             in File (fst fw, file) : mapDir setup' fn xs
                           Folder (p,dir) -> Folder (p, (mapDir setup fn dir)) : mapDir setup fn xs
mapDir _     _  []     = []

mapDirM :: DirM -> Directory -> IO ()
mapDirM act (x:xs) = case x of
                       File   cf      -> do act cf
                       Folder (_,dir) -> do mapDirM act dir
                                            mapDirM act xs
mapDirM _   []     = return ()

 -- DirM

type DirM = FileWPath -> IO ()

 -- File

data File = Undone   C.ByteString
          | Lexed    [Tok2]
          | Indented Indent
          | Parsed   Code
          | Compiled NIL
          deriving (Read, Show)

 -- Fixity

 -- Function

 -- FunctionType

 -- Indent

data Indent = Line Int [Tok1]
            | Indent [Indent]
            deriving (Read, Show)

colOf  (Line _ xs) = tok1Col (head xs)
colOf  (Indent ys) = colOf (head ys)
lineOf (Line ln _) = ln
lineOf (Indent ys) = lineOf (head ys)

 -- Lib

type Lib = ()

 -- Local

 -- NIL

 -- Pat

 -- Pattern

 -- PMonad

 -- Prim

 -- Setup

type Setup = [(String,Lib)]

 -- SpecialParse

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
         | Vartype       String'
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
tokString (Vartype s')    = s'

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

tok2Line, tok2Col :: Tok2 -> Int
tok2Line (ln,_,_) = ln
tok2Col  (_,cl,_) = cl

tok2Tok :: Tok2 -> Tok
tok2Tok (_,_,tk) = tk

 -- TokP

 -- Type
