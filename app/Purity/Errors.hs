{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Purity.Errors (prettyPrintError, prettyPrintErrorStr, handleError) where

import Control.Lens
import Control.Monad.Catch

import Data.List (intercalate)

import Purity.Types

import System.Exit

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

handleError :: SomeException -> Purity () 
handleError = \case 
    (fromException -> Just ExitSuccess) -> liftIO exitSuccess
    (fromException -> Just e@(UnknownError {})) -> prettyPrintError e
    (fromException -> Just e@(NotAllowed   {})) -> prettyPrintError e 
    (fromException -> Just e@(GhcException {})) -> prettyPrintError e 
    (fromException -> Just e@(WontCompile  {})) -> prettyPrintError e
    e -> prettyPrintErrorStr $ "Interpreter Exception: " ++ show @SomeException e
