{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Purity.Stmt (runLine, purityStmt) where

import Control.Lens
import Control.Monad.Catch

import Purity.Types
import Purity.Imports
import Purity.Errors
import Purity.TypeInfo
import Purity.Decls
import Purity.Cmd
import Purity.TermMode
import Purity.Directive

import System.Directory
import System.Exit
import System.Process

runLine :: String -> Purity () 
runLine input = gets (view (intSettings.termMode)) >>= \case
    CodeMode    -> runLineAsCode    input
    CommandMode -> runLineAsCommand input

runLineAsCode :: String -> Purity () 
runLineAsCode input = catch (purityStmt input) handleError

runLineAsCommand :: String -> Purity () 
runLineAsCommand input = do 
    formatted <- formatInput input
    catch @_ @SomeException (purityStmt formatted) $ \e -> do -- Try to run as command with auto formatting
        catch @_ @SomeException (purityStmt input) $ \_ -> do -- Try to run as command without auto formatting
            catch @_ @SomeException (runLineAsExternal input) (const $ handleError e) -- Try to run as external command

            -- If all fails, throw the original error from running with auto formatting

runLineAsExternal :: String -> Purity () 
runLineAsExternal cmd = do 
    let (x:xs) = words cmd
    (_, out, err) <- liftIO $ readProcessWithExitCode x xs ""

    liftIO (findExecutable x) >>= \case 
        Just _  -> liftIO (putStr out) >> prettyPrintErrorStr err
        Nothing -> throwM $ UnknownError $ "External command not found: " ++ x

purityStmt :: String -> Purity ()
purityStmt = \case 
    "\\"         -> toggleTermMode
    ":q"         -> liftIO exitSuccess -- For the vim users
    "#quit"      -> liftIO exitSuccess 
    "#purge"     -> reset >> (intImports .= [])
    "#mode code" -> setTermMode CodeMode
    "#mode cmd"  -> setTermMode CommandMode
    ('#' : xs) 
        | directive xs "importQ " -> purityImportQ (parseImportQList $ tail $ words xs)
        | directive xs "import "  -> purityImportStr $ tail $ words xs
        | directive xs "type "    -> printType $ concat $ tail $ words xs
        | directive xs "kind "    -> printKind $ concat $ tail $ words xs
        | directive xs "source "  -> mapM_ sourceFile $ tail $ words xs
        | otherwise -> prettyPrintErrorStr $ "Unknown directive: #" ++ xs
    (':' : x : xs) 
        | x == 't' -> printType xs
        | x == 'k' -> printKind xs
        | otherwise -> prettyPrintErrorStr $ "Unknown directive: :" ++ [x]
    "```" -> codeBlock ""
    xs -> runStmtOrDecls xs

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
