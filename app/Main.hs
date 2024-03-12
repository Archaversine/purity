{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Lens

import Control.Monad.IO.Class
import Control.Monad.State

import Data.Maybe

import Purity.Types
import Purity.Imports 
import Purity.Prompt
import Purity.Stmt

import System.IO

purityLoop :: Purity ()
purityLoop = promptUser >>= purityRunLine >> purityLoop

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
