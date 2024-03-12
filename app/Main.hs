{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad.State

import Purity

import System.IO

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering

    result <- runInterpreter (evalStateT purity defaultState)

    case result of 
        Left err -> putStrLn $ "Impossible Error: " ++ show err
        Right () -> return ()
