{-# LANGUAGE LambdaCase #-}

module Purity.Cmd where

import Control.Lens

import Data.Char

import Purity.Types

formatInput :: String -> Purity String
formatInput input = gets (view (intSettings.termMode)) >>= \case
    CodeMode    -> return input 
    CommandMode -> case words input of 
        []     -> return "" 
        [x]    -> return x
        (x:xs) -> return $ unwords (x : formatWords xs)

formatWords :: [String] -> [String]
formatWords [] = [] 
formatWords ([]:xs) = formatWords xs
formatWords (x:xs) 
    | all isNumber x   = x      : next
    | isUpper (head x) = x      : next
    | otherwise        = show x : next
    where next = formatWords xs
