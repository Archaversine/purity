module Purity.Decls where 

import Purity.Types

import qualified GHC

runDecls :: MonadInterpreter m => String -> m ()
runDecls s = runGhc $ void $ GHC.runDecls s
