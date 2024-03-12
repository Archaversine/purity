{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Purity.Types ( PurityState, intImports, intSettings
                    , TermSettings, termPrompt, termBlock, termErrClr, termMode
                    , TerminalMode(..)
                    , defaultTermSettings
                    , defaultState
                    , Purity
                    , module Language.Haskell.Interpreter 
                    , module Control.Monad.State
                    ) where

import Control.Lens
import Control.Monad.State

import Language.Haskell.Interpreter hiding (set, get)

data PurityState = PurityState { _intImports  :: ![ModuleImport] 
                               , _intSettings :: !TermSettings 
                               }

data TermSettings = TermSettings { _termPrompt :: !String  
                                 , _termBlock  :: !String
                                 , _termErrClr :: !String 
                                 , _termMode   :: !TerminalMode
                                 }

data TerminalMode = CodeMode | CommandMode deriving Eq

makeLenses ''PurityState
makeLenses ''TermSettings

defaultTermSettings :: TermSettings 
defaultTermSettings = TermSettings "Purity > " "" "" CommandMode

defaultState :: PurityState 
defaultState = PurityState [] defaultTermSettings

type Purity = StateT PurityState (InterpreterT IO)

instance MonadInterpreter Purity where
    fromSession          = lift . fromSession
    modifySessionRef t f = lift $ modifySessionRef t f
    runGhc x             = lift $ runGhc x
