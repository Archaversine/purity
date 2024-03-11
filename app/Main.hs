{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State

import Data.List

import System.Exit
import System.IO

import qualified Language.Haskell.Interpreter as Hint

newtype InterpreterState = IntState { intImports :: [String] }

instance Semigroup InterpreterState where 
    IntState a <> IntState b = IntState (a <> b) 

instance Monoid InterpreterState where 
    mempty = IntState []

type Interpreter = StateT InterpreterState (Hint.InterpreterT IO)

instance Hint.MonadInterpreter Interpreter where
    fromSession          = lift . Hint.fromSession
    modifySessionRef t f = lift $ Hint.modifySessionRef t f
    runGhc x             = lift $ Hint.runGhc x

purityImport :: [String] -> Interpreter ()
purityImport xs = do 
    curr <- get 
    let new@(IntState ys) = curr <> IntState xs
    put new >> Hint.setImports ys

purityLoop :: Interpreter ()
purityLoop = do 
    input <- liftIO (putStr "Purity > " >> getLine) 
    catch (purityStmt input) (liftIO . print @Hint.InterpreterError)
    purityLoop

purityStmt :: String -> Interpreter ()
purityStmt = \case 
    ":q"     -> liftIO exitSuccess -- For the vim users
    "#quit"  -> liftIO exitSuccess 
    "#purge" -> Hint.reset 
    ('#' : xs) 
        | "import" `isPrefixOf` xs -> purityImport (tail $ words xs) 
        | otherwise -> liftIO $ putStrLn $ "Unknown directive: " ++ xs
    xs -> Hint.runStmt xs

purity :: Interpreter ()
purity = purityImport ["Prelude"] >> purityLoop

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering
    putStrLn "PURITY SHELL [ALPHA]\n"

    result <- Hint.runInterpreter (evalStateT purity mempty)

    case result of 
        Left err -> putStrLn $ "Error: " ++ show err
        Right () -> return ()
