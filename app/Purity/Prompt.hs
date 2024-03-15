module Purity.Prompt (getPrompt, promptUser) where

import Data.Maybe

import Control.Lens

import Purity.Types
import Purity.Directory

import System.Console.Haskeline

getPrompt :: Purity (FilePath -> String)
getPrompt = do 
    settings <- get

    return $ case settings^.intSettings.termMode of 
        CodeMode    -> settings^.intSettings.termCodePrompt
        CommandMode -> settings^.intSettings.termCmdPrompt

purityGetLine :: String -> Purity (Maybe String)
purityGetLine = lift . getInputLine

promptUser :: Purity String 
promptUser = do 
    dir    <- getPurityCWD
    prompt <- getPrompt 
    input  <- purityGetLine (prompt dir)

    return (fromMaybe "" input)

