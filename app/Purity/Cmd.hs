{-# LANGUAGE LambdaCase #-}

module Purity.Cmd where

import Control.Lens

import Data.Char
import Data.List

import Purity.Types
import Purity.Directive
import Purity.Directory

import System.FilePath
import System.Directory

haskellSymbolChar :: Char -> Bool 
haskellSymbolChar = (`elem` "!#$%&*+./<=>?@\\^|-~")

-- Same as `words` but anything inside quotes is treated as a single word
wordsAndStrings :: String -> [String] 
wordsAndStrings [] = [] 
wordsAndStrings ('"':xs) = let (word, rest) = break (== '"') xs in 
   show word : wordsAndStrings (dropWhile isSpace $ drop 1 rest)
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
        (x:xs) -> unwords . (x:) <$> formatWords xs

formatWords :: [String] -> Purity [String]
formatWords [] = return [] 
formatWords ([]:xs) = formatWords xs
formatWords (x:xs) 
    | "."  == x                       = ret' (show <$> getPurityCWD)
    | ".." == x                       = ret' (show . takeDirectory <$> getPurityCWD)
    | "./"  `isPrefixOf` x            = ret' (show <$> liftIO (makeAbsolute x))
    | "../" `isPrefixOf` x            = ret' (show <$> liftIO (makeAbsolute x))
    | '"'  == head x && '"' == last x = ret x
    | '\'' == head x                  = ret (tail x)
    | '('  == head x && ')' == last x = ret x
    | all isNumber x                  = ret x
    | all haskellSymbolChar x         = ret x
    | isUpper (head x)                = ret x
    | otherwise                       = ret (show x)
    where next   = formatWords xs
          ret a  = (a:) <$> next
          ret' a = (:) <$> a <*> next
