module Purity.Directory (getPurityCWD) where 

import Purity.Types

getPurityCWD :: Purity FilePath
getPurityCWD = do 
    runStmt   "__cwd <- getCurrentDirectory"
    interpret "__cwd" (as :: FilePath)
