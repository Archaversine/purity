module Purity.TermMode (toggleTermMode, setTermMode, setTermModeStr) where

import Control.Lens

import Data.Char

import Purity.Types

toggleTermMode :: Purity ()
toggleTermMode = do 
    mode <- gets $ view (intSettings.termMode)

    let newMode = case mode of 
            CodeMode    -> CommandMode
            CommandMode -> CodeMode

    intSettings.termMode .= newMode

setTermMode :: TerminalMode -> Purity () 
setTermMode mode = intSettings.termMode .= mode

setTermModeStr :: String -> Purity () 
setTermModeStr xs = case map toLower xs of 
    "code" -> setTermMode CodeMode
    "cmd"  -> setTermMode CommandMode
    _      -> pure ()
