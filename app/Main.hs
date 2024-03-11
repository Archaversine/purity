{-# LANGUAGE LambdaCase #-}
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

data PurityState = PurityState { _intImports  :: [ModuleImport] 
                               , _intSettings :: TermSettings 
                               }

data TermSettings = TermSettings { _termPrompt :: String  
                                 , _termErrClr :: String 
                                 }

makeLenses ''PurityState
makeLenses ''TermSettings

defaultTermSettings :: TermSettings 
defaultTermSettings = TermSettings "Purity > " ""

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

purityLoop :: Purity ()
purityLoop = do 
    prompt <- gets $ view (intSettings.termPrompt)

    input <- liftIO (putStr prompt >> getLine) 
    catch (purityStmt input) prettyPrintError
    purityLoop

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

purityStmt :: String -> Purity ()
purityStmt = \case 
    ":q"     -> liftIO exitSuccess -- For the vim users
    "#quit"  -> liftIO exitSuccess 
    "#purge" -> reset 
    ('#' : xs) 
        | "importQ " `isPrefixOf` xs -> purityImportQ (parseImportQList $ tail $ words xs)
        | "import "  `isPrefixOf` xs -> purityImportStr $ tail $ words xs
        | "type "    `isPrefixOf` xs -> printType $ concat $ tail $ words xs
        | otherwise -> prettyPrintErrorStr $ "Unknown directive: #" ++ xs
    (':' : x : xs) 
        | x == 't' -> printType $ unwords $ words xs
        | otherwise -> prettyPrintErrorStr $ "Unknown directive: :" ++ [x]
    xs -> runStmt xs

purity :: Purity ()
purity = do 
    loadModules ["Config.hs"]
    purityImport [ModuleImport "Config" (QualifiedAs (Just "Config")) NoImportList]

    prompt <- interpret "Config.shellPrompt" (as :: String)
    errClr <- fromMaybe "" <$> interpret "Config.errorColorPrefix" (as :: Maybe String)

    let settings = defaultTermSettings & termPrompt .~ prompt
                                       & termErrClr .~ errClr

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
