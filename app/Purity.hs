{-# LANGUAGE LambdaCase #-}

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

import System.Console.Haskeline
import System.Environment
import System.Directory
import System.FilePath

purityLoop :: Purity ()
purityLoop = promptUser >>= runLine >> purityLoop

loadConfig :: Purity ()
loadConfig = do 
    cmdPrompt  <- interpret "Config.commandPrompt" (as :: String)
    codePrmopt <- interpret "Config.codePrompt"    (as :: String)
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
    configFile <- liftIO $ doesFileExist (path </> "Config.hs") >>= \case 
        True  -> return (path </> "Config.hs")
        False -> return (cwd  </> "Config.hs")

    -- Prefer to be in same directory as the executable
    userFile   <- liftIO $ doesFileExist (path </> "User.hs") >>= \case 
        True  -> return (path </> "User.hs")
        False -> return (cwd  </> "User.hs")

    loadModules  [configFile, userFile]
    purityImport [ModuleImport "Config" (QualifiedAs (Just "Config")) NoImportList]
    purityImport [ModuleImport "User"   NotQualified                  NoImportList]

    loadConfig

    interpret "Config.splashText"     (as :: Maybe String) >>= liftIO . putStrLn . fromMaybe ""
    interpret "Config.defaultImports" (as :: [String]    ) >>= purityImportStr >> purityLoop

runPurity :: MonadIO m => Purity a -> m (Either InterpreterError a) 
runPurity p = liftIO $ runInterpreter $ runInputT defaultSettings (evalStateT p defaultState)
