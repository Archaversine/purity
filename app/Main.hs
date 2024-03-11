module Main where

import Control.Monad.IO.Class

import System.IO

import qualified Language.Haskell.Interpreter as Hint

purityImports :: Hint.InterpreterT IO ()
purityImports = Hint.setImports ["Prelude"]

purityLoop :: Hint.InterpreterT IO () 
purityLoop = liftIO (putStr "Purity > " >> getLine) >>= Hint.runStmt >> purityLoop

purity :: Hint.InterpreterT IO ()
purity = purityImports >> purityLoop

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering
    putStrLn "PURITY SHELL [ALPHA]\n"

    result <- Hint.runInterpreter purity 

    case result of 
        Left err -> putStrLn $ "Error: " ++ show err
        Right () -> return ()
