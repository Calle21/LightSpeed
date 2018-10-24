module ToString (putAllIndent, writeIndent) where

import qualified Data.ByteString.Char8 as C
import Type.Token
import Type.DirM
import Type.File(File(Indented))
import Type.Indent
import Ubi
import Util(isVisibleDirectory, listDirectory')
import System.FilePath.Posix (takeBaseName, takeDirectory, takeFileName, (</>))

tokenToString :: Token -> C.ByteString
tokenToString t = case t of
                  Keyword s     -> C.pack s
                  Opname s      -> C.pack s
                  Option s      -> C.pack s
                  Punctuation c -> C.pack [c]
                  Reserved s    -> C.pack s
                  Special s     -> C.pack s
                  TokenChar c   -> C.pack $ show c
                  TokenFloat f  -> C.pack $ show f
                  TokenInt n    -> C.pack $ show n
                  TokenString s -> C.pack $ show s
                  Type s        -> C.pack s
                  Vartype s     -> C.pack s

indentToString :: Indent -> C.ByteString
indentToString (Line _ xs) = C.replicate (pred (fst $ head xs)) ' ' `C.append` C.pack " " `C.intercalate` ((tokenToString . snd) `map` xs) `C.append` C.pack "\n"
indentToString (Indent ys) | colOfIndent (head ys) == 1 = separate $ indentToString `map` ys
                           | otherwise                  = C.concat (indentToString `map` ys)
  where
  separate :: [C.ByteString] -> C.ByteString
  separate (x:y:xs) = x `C.append` (if C.head y /= ' ' then C.pack "\n" else C.empty) `C.append` separate (y:xs)
  separate (x:_)    = x

writeIndent :: DirM
writeIndent (path,file) = case file of
                          Indented y -> do let thebs   = indentToString y
                                               hidpath = takeDirectory path </> ".tostring" </> takeBaseName path
                                           C.writeFile hidpath thebs

putAllIndent :: FilePath -> IO ()
putAllIndent path = do contents <- listDirectory' path
                       maps <- filterM isVisibleDirectory contents
                       mapM_ putAllIndent maps
                       this <- listDirectory' (path </> ".tostring")
                       mapM_ (\path' -> do putStrLn (path </> takeFileName path' ++ ".nova")
                                           bs <- C.readFile path'
                                           C.putStrLn bs)
                             this

