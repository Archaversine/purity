{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Purity.Stmt where

import Control.Lens
import Control.Monad.Catch

import Data.List

import Purity.Types
import Purity.Imports
import Purity.Errors
import Purity.TypeInfo
import Purity.Decls

import System.Exit

purityRunLine :: String -> Purity () 
purityRunLine input = catch (catch (purityStmt input) prettyPrintError) $ \case
    (fromException -> Just ExitSuccess) -> liftIO exitSuccess -- rethrow exit success to actually exit the program
    e -> prettyPrintErrorStr $ "Interpreter Exception: " ++ show @SomeException e

purityStmt :: String -> Purity ()
purityStmt = \case 
    ":q"     -> liftIO exitSuccess -- For the vim users
    "#quit"  -> liftIO exitSuccess 
    "#purge" -> reset >> (intImports .= [])
    ('#' : xs) 
        | "importQ " `isPrefixOf` xs -> purityImportQ (parseImportQList $ tail $ words xs)
        | "import "  `isPrefixOf` xs -> purityImportStr $ tail $ words xs
        | "type "    `isPrefixOf` xs -> printType $ concat $ tail $ words xs
        | "kind "    `isPrefixOf` xs -> printKind $ concat $ tail $ words xs
        | "source "  `isPrefixOf` xs -> mapM_ puritySourceFile $ tail $ words xs
        | otherwise -> prettyPrintErrorStr $ "Unknown directive: #" ++ xs
    (':' : x : xs) 
        | x == 't' -> printType xs
        | x == 'k' -> printKind xs
        | otherwise -> prettyPrintErrorStr $ "Unknown directive: :" ++ [x]
    "```" -> purityCodeBlock ""
    xs
        | "data "     `isPrefixOf` xs -> runDecls xs
        | "class "    `isPrefixOf` xs -> runDecls xs
        | "newtype "  `isPrefixOf` xs -> runDecls xs
        | "instance " `isPrefixOf` xs -> runDecls xs
        | otherwise -> runStmt xs

puritySourceFile :: FilePath -> Purity () 
puritySourceFile path = liftIO (lines <$> readFile path) >>= puritySourceLine ""

puritySourceLine :: String -> [String] -> Purity ()
puritySourceLine _ [] = return ()
puritySourceLine [] ("```":ys) = puritySourceLine " " ys -- empty string means start of codeblock
puritySourceLine xs ("```":ys) = purityRunLine xs >> puritySourceLine "" ys
puritySourceLine [] (y  :  ys) = purityRunLine y >> puritySourceLine "" ys -- empty string means not in codeblock
puritySourceLine xs (y  :  ys) = puritySourceLine (xs <> "\n" <> y) ys -- not empty string means in codeblock

purityCodeBlock :: String -> Purity () 
purityCodeBlock curr = do 
    block <- gets   $ view (intSettings.termBlock)
    input <- liftIO $ putStr block >> getLine

    case input of 
        "```" -> purityRunLine curr
        _     -> purityCodeBlock (curr <> "\n" <> input)
