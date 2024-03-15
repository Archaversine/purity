{-# LANGUAGE TypeApplications #-}

module Purity ( module Purity.Types 
              , module Purity.Imports 
              , module Purity.Prompt 
              , module Purity.Stmt
              , purity 
              , purityLoop
              , runPurity
              ) where 

import Control.Lens
import Control.Monad.Catch

import Data.Maybe

import Purity.Types
import Purity.Imports 
import Purity.Prompt
import Purity.Stmt
import Purity.Directory

import System.Console.Haskeline
import System.Environment
import System.Directory
import System.FilePath

purityLoop :: Purity ()
purityLoop = promptUser >>= runLine >> purityLoop

loadConfig :: Purity ()
loadConfig = do 
    cmdPrompt  <- interpret "Config.commandPrompt" (as :: FilePath -> String)
    codePrmopt <- interpret "Config.codePrompt"    (as :: FilePath -> String)
    errClr     <- fromMaybe "" <$> interpret "Config.errorColorPrefix" (as :: Maybe String)
    block      <- fromMaybe "" <$> interpret "Config.blockPrompt"      (as :: Maybe String)

    let settings = defaultTermSettings & termCmdPrompt  .~ cmdPrompt
                                       & termCodePrompt .~ codePrmopt
                                       & termErrClr     .~ errClr
                                       & termBlock      .~ block

    intSettings .= settings

data ConfigFileNotFound = ConfigFileNotFound FilePath [FilePath]

instance Show ConfigFileNotFound where 
    show (ConfigFileNotFound path ps) = "Config file " ++ path ++ " not found in any of the following directories:\n" ++ unlines ps

instance Exception ConfigFileNotFound

findPurityConfigFile' :: [FilePath] -> [FilePath] -> FilePath -> Purity FilePath
findPurityConfigFile' ps [] path = throwM $ ConfigFileNotFound path ps
findPurityConfigFile' ps (x:xs) path = do 
    exists <- liftIO $ doesFileExist (x </> path)
    if exists then return (x </> path) else findPurityConfigFile' ps xs path

findPurityConfigFile :: [FilePath] -> FilePath -> Purity FilePath
findPurityConfigFile ps = findPurityConfigFile' ps ps

purity :: Purity ()
purity = do 
    path <- liftIO getExecutablePath
    home <- liftIO getHomeDirectory
    cwd  <- liftIO getCurrentDirectory

    -- Order to search for config files
    let dirSearchOrder = [path, home </> ".purity", cwd]

    configFile  <- findPurityConfigFile dirSearchOrder "Config.hs"
    userFile    <- findPurityConfigFile dirSearchOrder "User.hs"
    builtinFile <- findPurityConfigFile dirSearchOrder "Builtin.hs" 

    loadModules  [configFile, userFile, builtinFile]
    purityImport [ModuleImport "Config"  (QualifiedAs (Just "Config")) NoImportList]
    purityImport [ModuleImport "User"    NotQualified                  NoImportList]
    purityImport [ModuleImport "Builtin" NotQualified                  NoImportList]

    loadConfig

    interpret "Config.splashText"     (as :: Maybe String) >>= liftIO . putStrLn . fromMaybe ""
    interpret "Config.defaultImports" (as :: [String]    ) >>= purityImportStr >> purityLoop

runPurity :: MonadIO m => Purity a -> m (Either InterpreterError a) 
runPurity p = liftIO $ runInterpreter $ runInputT inputSettings (evalStateT p defaultState)
    where inputSettings = (defaultSettings @Purity) { complete = autoCompleteFileName }

autoCompleteFileName :: CompletionFunc (InterpreterT IO)
autoCompleteFileName input = do 
    cwd <- getPurityCWDInterpreter
    liftIO $ withCurrentDirectory cwd (completeFilename input)
