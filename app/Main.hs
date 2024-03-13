{-# LANGUAGE LambdaCase #-}

module Main where

import Purity

import System.IO

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering
    runPurity purity >>= \case 
        Left err -> putStrLn $ "Unrecoverable Crash: " ++ show err
        Right () -> return ()
