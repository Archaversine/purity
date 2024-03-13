module Purity ( module Purity.Types 
              , module Purity.Imports 
              , module Purity.Prompt 
              , module Purity.Stmt
              , purity 
              , purityLoop
              ) where 

import Control.Lens

import Data.Maybe

import Purity.Types
import Purity.Imports 
import Purity.Prompt
import Purity.Stmt

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
    loadModules ["Config.hs", "User.hs"]
    purityImport [ModuleImport "Config" (QualifiedAs (Just "Config")) NoImportList]
    purityImport [ModuleImport "User"   NotQualified                  NoImportList]

    loadConfig

    interpret "Config.splashText"     (as :: Maybe String) >>= liftIO . putStrLn . fromMaybe ""
    interpret "Config.defaultImports" (as :: [String]    ) >>= purityImportStr >> purityLoop
