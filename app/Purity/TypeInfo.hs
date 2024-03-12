module Purity.TypeInfo (printType, printKind) where

import Control.Monad.IO.Class

import Purity.Types

printType :: String -> Purity ()
printType xs = do 
    info <- typeOf xs
    liftIO $ putStrLn $ xs ++ " :: " ++ info

printKind :: String -> Purity () 
printKind xs = do 
    info <- kindOf xs
    liftIO $ putStrLn $ xs ++ " :: " ++ info
