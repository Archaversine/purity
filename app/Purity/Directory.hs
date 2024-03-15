module Purity.Directory ( getPurityCWD
                        , getPurityCWDInterpreter
                        , makeAbsolute
                        ) where 

import Purity.Types

import System.FilePath

getPurityCWD :: Purity FilePath
getPurityCWD = lift (lift getPurityCWDInterpreter)

getPurityCWDInterpreter :: InterpreterT IO FilePath
getPurityCWDInterpreter = do 
    runStmt   "__cwd <- getCurrentDirectory"
    interpret "__cwd" (as :: FilePath)

makeAbsolute :: FilePath -> Purity FilePath
makeAbsolute path = do 
    cwd <- getPurityCWD
    return (cwd </> path)
