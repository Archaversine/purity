module Purity.Prompt where

import Control.Lens

import Purity.Types

printPrompt :: Purity () 
printPrompt = gets (view (intSettings.termPrompt)) >>= liftIO . putStr

promptUser :: Purity String 
promptUser = printPrompt >> liftIO getLine
