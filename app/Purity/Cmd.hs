{-# LANGUAGE LambdaCase #-}

module Purity.Cmd where

import Control.Lens

import Data.Char

import Purity.Types
import Purity.Directive

haskellSymbolChar :: Char -> Bool 
haskellSymbolChar = (`elem` "!#$%&*+./<=>?@\\^|-~")

-- Same as `words` but anything inside quotes is treated as a single word
wordsAndStrings :: String -> [String] 
wordsAndStrings [] = [] 
wordsAndStrings ('"':xs) = let (word, rest) = break (== '"') xs in 
    ('"' : word ++ "\"") : wordsAndStrings (dropWhile isSpace $ drop 1 rest)
wordsAndStrings xs = let (word, rest) = break isSpace xs in
    word : wordsAndStrings (dropWhile isSpace rest)

formatInput :: String -> Purity String
formatInput input 
    | isDirective input || input == "```" = return input
    | otherwise = gets (view (intSettings.termMode)) >>= \case
    CodeMode    -> return input 
    CommandMode -> case wordsAndStrings input of 
        []     -> return "" 
        [x]    -> return x
        (x:xs) -> return $ unwords (x : formatWords xs)

formatWords :: [String] -> [String]
formatWords [] = [] 
formatWords ([]:xs) = formatWords xs
formatWords (x:xs) 
    | "." == x                       = show x : next
    | ".." == x                      = show x : next
    | '"' == head x                  = x      : next
    | '(' == head x && ')' == last x = x      : next
    | all isNumber x                 = x      : next
    | all haskellSymbolChar x        = x      : next
    | isUpper (head x)               = x      : next
    | otherwise                      = show x : next
    where next = formatWords xs
