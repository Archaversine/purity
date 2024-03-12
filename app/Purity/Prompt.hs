module Purity.Prompt (printPrompt, promptUser) where

import Control.Lens

import Purity.Types

printPrompt :: Purity () 
printPrompt = do 
    settings <- get

    liftIO $ putStr $ case settings^.intSettings.termMode of 
        CodeMode    -> settings^.intSettings.termCodePrompt
        CommandMode -> settings^.intSettings.termCmdPrompt

promptUser :: Purity String 
promptUser = printPrompt >> liftIO getLine
