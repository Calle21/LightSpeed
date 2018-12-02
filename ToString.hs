module ToString where

import qualified Data.ByteString.Char8 as C
import Types
import Ubi
import Util

tokToString :: Tok -> String'
tokToString t = case t of
                  Keyword s     -> s
                  Opname s      -> s
                  Option s      -> s
                  Punctuation c -> C.pack [c]
                  Reserved s    -> s
                  Special s     -> s
                  TokChar c     -> C.pack $ show c
                  TokFloat f    -> C.pack $ show f
                  TokInt n      -> C.pack $ show n
                  TokString s   -> C.pack $ show $ C.unpack s
                  Type s        -> s
                  Typevar s     -> s

indentToString :: Line -> String'
indentToString (Line _ xs) = C.replicate (pred (fst $ head xs)) ' ' `C.append` C.pack " " `C.intercalate` ((tokToString . snd) `map` xs) `C.append` C.pack "\n"
indentToString (Fold ys) | colOf (head ys) == 1 = separate $ indentToString `map` ys
                         | otherwise                  = C.concat (indentToString `map` ys)
  where
  separate :: [String'] -> String'
  separate (x:y:xs) = x `C.append` (if C.head y /= ' ' then C.pack "\n" else C.empty) `C.append` separate (y:xs)
  separate (x:_)    = x

writeIndent :: DirM
writeIndent path file = case file of
                          Indented y -> do let thebs   = indentToString y
                                               hidpath = takeDirectory path </> ".tostring" </> takeBaseName path
                                           C.writeFile hidpath thebs

putAllIndent :: FilePath -> IO ()
putAllIndent path = do contents <- listDirectory' path
                       maps <- filterM isPathVisibleDirectory contents
                       mapM_ putAllIndent maps
                       this <- listDirectory' (path </> ".tostring")
                       mapM_ (\path' -> do putStrLn (path </> takeFileName path' ++ ".nova")
                                           bs <- C.readFile path'
                                           C.putStrLn bs)
                              this

