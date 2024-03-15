{-# LANGUAGE LambdaCase #-}
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

purity :: Purity ()
purity = do 
    cwd  <- liftIO getCurrentDirectory
    path <- liftIO getExecutablePath

    -- Prefer to be in same directory as the executable
    configFile  <- liftIO $ doesFileExist (path </> "Config.hs") >>= \case 
        True  -> return (path </> "Config.hs")
        False -> return (cwd  </> "Config.hs")

    -- Prefer to be in same directory as the executable
    userFile    <- liftIO $ doesFileExist (path </> "User.hs") >>= \case 
        True  -> return (path </> "User.hs")
        False -> return (cwd  </> "User.hs")

    builtinFile <- liftIO $ doesFileExist (path </> "Builtin.hs") >>= \case 
        True  -> return (path </> "Builtin.hs")
        False -> return (cwd  </> "Builtin.hs")

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
