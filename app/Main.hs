{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.State

import Purity

import System.IO

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering
    runInterpreter (evalStateT purity defaultState) >>= \case
        Left err -> putStrLn $ "Unrecoverable Crash: " ++ show err
        Right () -> return ()
