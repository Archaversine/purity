{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Purity.Stmt (runLine, purityStmt) where

import Control.Lens
import Control.Monad.Catch

import Data.List

import Purity.Types
import Purity.Imports
import Purity.Errors
import Purity.TypeInfo
import Purity.Decls

import System.Exit

runLine :: String -> Purity () 
runLine input = catch (catch (purityStmt input) prettyPrintError) $ \case
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
        | "source "  `isPrefixOf` xs -> mapM_ sourceFile $ tail $ words xs
        | otherwise -> prettyPrintErrorStr $ "Unknown directive: #" ++ xs
    (':' : x : xs) 
        | x == 't' -> printType xs
        | x == 'k' -> printKind xs
        | otherwise -> prettyPrintErrorStr $ "Unknown directive: :" ++ [x]
    "```" -> codeBlock ""
    xs
        | "data "     `isPrefixOf` xs -> runDecls xs
        | "class "    `isPrefixOf` xs -> runDecls xs
        | "newtype "  `isPrefixOf` xs -> runDecls xs
        | "instance " `isPrefixOf` xs -> runDecls xs
        | otherwise -> runStmt xs

sourceFile :: FilePath -> Purity () 
sourceFile path = liftIO (lines <$> readFile path) >>= sourceFileLine ""

sourceFileLine :: String -> [String] -> Purity ()
sourceFileLine _ [] = return ()
sourceFileLine [] ("```":ys) = sourceFileLine " " ys -- empty string means start of codeblock
sourceFileLine xs ("```":ys) = runLine xs >> sourceFileLine "" ys
sourceFileLine [] (y  :  ys) = runLine y >> sourceFileLine "" ys -- empty string means not in codeblock
sourceFileLine xs (y  :  ys) = sourceFileLine (xs <> "\n" <> y) ys -- not empty string means in codeblock

codeBlock :: String -> Purity () 
codeBlock curr = do 
    block <- gets   $ view (intSettings.termBlock)
    input <- liftIO $ putStr block >> getLine

    case input of 
        "```" -> runLine curr
        _     -> codeBlock (curr <> "\n" <> input)
