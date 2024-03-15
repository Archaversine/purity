module Purity.Directory (getPurityCWD, makeAbsolute) where 

import Purity.Types

import System.FilePath

getPurityCWD :: Purity FilePath
getPurityCWD = do 
    runStmt   "__cwd <- getCurrentDirectory"
    interpret "__cwd" (as :: FilePath)

makeAbsolute :: FilePath -> Purity FilePath
makeAbsolute path = do 
    cwd <- getPurityCWD
    return (cwd </> path)
