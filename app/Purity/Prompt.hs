module Purity.Prompt (getPrompt, promptUser) where

import Data.Maybe

import Control.Lens

import Purity.Types

import System.Console.Haskeline
import System.Directory

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
    cwd    <- liftIO getCurrentDirectory
    prompt <- getPrompt 
    input  <- purityGetLine (prompt cwd)

    return (fromMaybe "" input)

