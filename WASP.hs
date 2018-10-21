module Nova.WASP (WASP, makeLib) where

import Nova.Compile
import Nova.Indent
import Nova.Lex
import Nova.List
import Nova.Parse
import System.Directory(createDirectoryIfMissing, getArgs, getCurrentDirectory, getHomeDirectory, makeAbsolute)
import System.FilePath.Posix ((</>))

WASP :: IO ()
WASP = do args <- getArgs
          let mklib = "makelib" `elem` args
          currentDir <- getCurrentDirectory
          files      <- getFiles currentDir
          let lexed    = mapDir lex     files
              indented = mapDir indent  lexed
              parsed   = mapDir parse   indented
              compiled = mapDir compile parsed
          putComp compiled
          if mklib then putLib =<< makeLib currentDir
                   else return ()

makeLib :: FilePath -> IO Lib
makeLib dir = undefined

putLib :: Lib -> IO ()
putLib lib = do home <- getHomeDirectory
                let libpath = home </> ".novalibs"
                createDirectoryIfMissing libpath
                writeFile (libpath </> fst lib) (show lib)
