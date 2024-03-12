module Purity.Errors (prettyPrintError, prettyPrintErrorStr) where

import Control.Lens

import Data.List (intercalate)

import Purity.Types

prettyPrintError :: InterpreterError -> Purity ()
prettyPrintError err = prettyPrintErrorStr $ case err of
    UnknownError e -> "Magical error: " ++ e
    NotAllowed   e -> "Not allowed: " ++ e
    GhcException e -> "GHC threw an exception: " ++ e
    WontCompile ms -> "Compilation Error\n\n" ++ intercalate "\n" (map errMsg ms)

prettyPrintErrorStr :: String -> Purity () 
prettyPrintErrorStr err = do 
    color <- gets $ view (intSettings.termErrClr) 
    liftIO $ putStr color >> putStrLn err >> putStr "\ESC[0m" -- Reset color
