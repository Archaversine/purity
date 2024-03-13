{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Purity.Types ( PurityState, intImports, intSettings
                    , TermSettings, termCmdPrompt, termCodePrompt, termBlock, termErrClr, termMode
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

import System.Console.Haskeline

data PurityState = PurityState { _intImports  :: ![ModuleImport] 
                               , _intSettings :: !TermSettings 
                               }

data TermSettings = TermSettings { _termCmdPrompt  :: !(FilePath -> String)
                                 , _termCodePrompt :: !(FilePath -> String)
                                 , _termBlock      :: !String
                                 , _termErrClr     :: !String 
                                 , _termMode       :: !TerminalMode
                                 }

data TerminalMode = CodeMode | CommandMode deriving Eq

makeLenses ''PurityState
makeLenses ''TermSettings

defaultTermSettings :: TermSettings 
defaultTermSettings = TermSettings defaultCmd defaultCode "" "" CommandMode
    where defaultCmd  = const "Purity > "
          defaultCode = const "Purity = "

defaultState :: PurityState 
defaultState = PurityState [] defaultTermSettings

type Purity = StateT PurityState (InputT (InterpreterT IO))

instance MonadInterpreter Purity where
    fromSession          = lift . lift . fromSession
    modifySessionRef t f = lift $ lift $ modifySessionRef t f
    runGhc x             = lift $ lift $ runGhc x
