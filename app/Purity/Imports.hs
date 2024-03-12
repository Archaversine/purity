{-# LANGUAGE TypeApplications #-}

module Purity.Imports where

import Control.Lens
import Control.Monad.Catch

import Data.Either (partitionEithers)
import Data.List (intercalate)

import Purity.Types
import Purity.Errors

parseImportList :: [String] -> [ModuleImport]
parseImportList = map (\n -> ModuleImport n NotQualified NoImportList)

parseImportQList :: [String] -> [Either String ModuleImport]
parseImportQList [] = []
parseImportQList (x:xs) = parsed : parseImportQList xs
    where parsed = case break (== ':') x of 
            (a, ':':b) -> Right $ ModuleImport a (QualifiedAs (Just b)) NoImportList
            _          -> Left x

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
