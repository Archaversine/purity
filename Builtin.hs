module Builtin where

import System.Process
import System.Directory

ls :: String -> IO () 
ls path = callCommand $ "ls " ++ path

cd :: FilePath -> IO ()
cd = setCurrentDirectory

cwd :: IO FilePath 
cwd = getCurrentDirectory

