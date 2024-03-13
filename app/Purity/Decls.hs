{-# LANGUAGE TypeApplications #-}

module Purity.Decls (runDecls, runStmtOrDecls) where 

import Data.Char
import Data.List

import Purity.Types

import qualified GHC

runDecls :: MonadInterpreter m => String -> m ()
runDecls s = runGhc $ void $ GHC.runDecls s

isDecl :: String -> Bool 
isDecl s 
    | "data "     `isPrefixOf` s' = True
    | "type "     `isPrefixOf` s' = True 
    | "newtype "  `isPrefixOf` s' = True 
    | "class "    `isPrefixOf` s' = True 
    | "instance " `isPrefixOf` s' = True 
    | "deriving " `isPrefixOf` s' = True 
    | "foreign "  `isPrefixOf` s' = True 
    | otherwise                  = False
    where s' = dropWhile isSpace s

runStmtOrDecls :: MonadInterpreter m => String -> m ()
runStmtOrDecls s = if isDecl s then runDecls s else runStmt s
