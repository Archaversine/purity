{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State

import Data.Either
import Data.List

import System.Exit
import System.IO

import Language.Haskell.Interpreter hiding (get)

newtype PurityState = PurityState { intImports :: [ModuleImport] }

instance Semigroup PurityState where 
    PurityState a <> PurityState b = PurityState (a <> b)

instance Monoid PurityState where 
    mempty = PurityState []

type Purity = StateT PurityState (InterpreterT IO)

instance MonadInterpreter Purity where
    fromSession          = lift . fromSession
    modifySessionRef t f = lift $ modifySessionRef t f
    runGhc x             = lift $ runGhc x

purityImport :: [ModuleImport] -> Purity ()
purityImport is = do 
    curr <- get 
    let new = curr <> PurityState is
    put new >> setImportsF (intImports new)

purityImportStr :: [String] -> Purity ()
purityImportStr = purityImport . map (\n -> ModuleImport n NotQualified NoImportList)

purityImportQ :: [Either String ModuleImport] -> Purity () 
purityImportQ xs = do 
    let (errs, mods) = partitionEithers xs

    unless (null errs) $ liftIO $ do 
        putStrLn "Could not import the following:\n"
        mapM_ putStrLn errs

    purityImport mods

purityLoop :: Purity ()
purityLoop = do 
    input <- liftIO (putStr "Purity > " >> getLine) 
    catch (purityStmt input) (liftIO . prettyPrintError)
    purityLoop

prettyPrintError :: InterpreterError -> IO ()
prettyPrintError = \case
    (UnknownError e) -> putStrLn $ "Magical error: " ++ e
    (NotAllowed   e) -> putStrLn $ "Not allowed: " ++ e
    (GhcException e) -> putStrLn $ "GHC threw an exception: " ++ e
    (WontCompile ms) -> putStrLn $ "Compilation Error\n\n" ++ intercalate "\n" (map errMsg ms)

parseImportList :: [String] -> [ModuleImport]
parseImportList = map (\n -> ModuleImport n NotQualified NoImportList)

parseImportQList :: [String] -> [Either String ModuleImport]
parseImportQList [] = []
parseImportQList (x:xs) = parsed : parseImportQList xs
    where parsed = case break (== ':') x of 
            (a, ':':b) -> Right $ ModuleImport a (QualifiedAs (Just b)) NoImportList
            _          -> Left x

purityStmt :: String -> Purity ()
purityStmt = \case 
    ":q"     -> liftIO exitSuccess -- For the vim users
    "#quit"  -> liftIO exitSuccess 
    "#purge" -> reset 
    ('#' : xs) 
        | "importQ" `isPrefixOf` xs -> purityImportQ (parseImportQList $ tail $ words xs)
        | "import"  `isPrefixOf` xs -> purityImportStr $ tail $ words xs
        | otherwise -> liftIO $ putStrLn $ "Unknown directive: " ++ xs
    xs -> runStmt xs

purity :: Purity ()
purity = do 
    loadModules ["Config.hs"]
    purityImport [ModuleImport "Config" (QualifiedAs (Just "Config")) NoImportList]
    interpret "Config.defaultImports" (as :: [String]) >>= purityImportStr >> purityLoop

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering
    putStrLn "PURITY SHELL [ALPHA]\n"

    result <- runInterpreter (evalStateT purity $ PurityState [])

    case result of 
        Left err -> putStrLn $ "Error: " ++ show err
        Right () -> return ()
