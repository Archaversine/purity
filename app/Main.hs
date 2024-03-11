{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Lens

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State

import Data.Either
import Data.List
import Data.Maybe

import System.Exit
import System.IO

import Language.Haskell.Interpreter hiding (get)

data PurityState = PurityState { _intImports  :: ![ModuleImport] 
                               , _intSettings :: !TermSettings 
                               }

data TermSettings = TermSettings { _termPrompt :: !String  
                                 , _termBlock  :: !String
                                 , _termErrClr :: !String 
                                 }

makeLenses ''PurityState
makeLenses ''TermSettings

defaultTermSettings :: TermSettings 
defaultTermSettings = TermSettings "Purity > " "" ""

defaultState :: PurityState 
defaultState = PurityState [] defaultTermSettings

type Purity = StateT PurityState (InterpreterT IO)

instance MonadInterpreter Purity where
    fromSession          = lift . fromSession
    modifySessionRef t f = lift $ modifySessionRef t f
    runGhc x             = lift $ runGhc x

purityImport :: [ModuleImport] -> Purity ()
purityImport is = do 
    curr <- gets (view intImports)
    let new = curr <> is

    catch @_ @InterpreterError (setImportsF new >> (intImports .= new)) $ \_ -> do 
        prettyPrintErrorStr "Import directive failed."
        prettyPrintErrorStr "Could not import the following:\n"
        prettyPrintErrorStr "-------------------------"
        mapM_ (prettyPrintErrorStr . show) is
        prettyPrintErrorStr "-------------------------"

purityImportStr :: [String] -> Purity ()
purityImportStr = purityImport . map (\n -> ModuleImport n NotQualified NoImportList)

purityImportQ :: [Either String ModuleImport] -> Purity () 
purityImportQ xs = do 
    let (errs, mods) = partitionEithers xs

    unless (null errs) $ do 
        prettyPrintErrorStr $ "Could not import the following:\n" ++ intercalate "\n" errs

    purityImport mods

printPrompt :: Purity () 
printPrompt = gets (view (intSettings.termPrompt)) >>= liftIO . putStr

promptUser :: Purity String 
promptUser = printPrompt >> liftIO getLine

purityLoop :: Purity ()
purityLoop = promptUser >>= purityRunLine >> purityLoop

purityRunLine :: String -> Purity () 
purityRunLine input = catch (catch (purityStmt input) prettyPrintError) $ \case
    (fromException -> Just ExitSuccess) -> liftIO exitSuccess -- rethrow exit success to actually exit the program
    e -> prettyPrintErrorStr $ "Interpreter Exception: " ++ show @SomeException e

prettyPrintError :: InterpreterError -> Purity ()
prettyPrintError err = prettyPrintErrorStr $ case err of
    UnknownError e -> "Magical error: " ++ e
    NotAllowed   e -> "Not allowed: " ++ e
    GhcException e -> "GHC threw an exception: " ++ e
    WontCompile ms -> "Compilation Error\n\n" ++ intercalate "\n" (map errMsg ms)

prettyPrintErrorStr :: String -> Purity () 
prettyPrintErrorStr err = do 
    color <- gets $ view (intSettings.termErrClr) 
    liftIO $ putStr color >> putStrLn err >> putStr "\ESC[0m" -- Reset color

parseImportList :: [String] -> [ModuleImport]
parseImportList = map (\n -> ModuleImport n NotQualified NoImportList)

parseImportQList :: [String] -> [Either String ModuleImport]
parseImportQList [] = []
parseImportQList (x:xs) = parsed : parseImportQList xs
    where parsed = case break (== ':') x of 
            (a, ':':b) -> Right $ ModuleImport a (QualifiedAs (Just b)) NoImportList
            _          -> Left x

printType :: String -> Purity ()
printType xs = do 
    info <- typeOf xs
    liftIO $ putStrLn $ xs ++ " :: " ++ info

printKind :: String -> Purity () 
printKind xs = do 
    info <- kindOf xs
    liftIO $ putStrLn $ xs ++ " :: " ++ info

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
    xs    -> runStmt xs

purity :: Purity ()
purity = do 
    loadModules ["Config.hs", "User.hs"]
    purityImport [ModuleImport "Config" (QualifiedAs (Just "Config")) NoImportList]
    purityImport [ModuleImport "User"   NotQualified                  NoImportList]

    prompt <- interpret "Config.shellPrompt" (as :: String)
    errClr <- fromMaybe "" <$> interpret "Config.errorColorPrefix" (as :: Maybe String)
    block  <- fromMaybe "" <$> interpret "Config.blockPrompt"      (as :: Maybe String)

    let settings = defaultTermSettings & termPrompt .~ prompt
                                       & termErrClr .~ errClr
                                       & termBlock  .~ block

    intSettings .= settings

    interpret "Config.splashText"     (as :: Maybe String) >>= liftIO . putStrLn . fromMaybe ""
    interpret "Config.defaultImports" (as :: [String]    ) >>= purityImportStr >> purityLoop

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering

    result <- runInterpreter (evalStateT purity defaultState)

    case result of 
        Left err -> putStrLn $ "Error: " ++ show err
        Right () -> return ()
