module Purity.Directive (isDirective, directive) where

import Data.List

isDirective :: String -> Bool 
isDirective ('#':_) = True 
isDirective [':',_] = True 
isDirective _       = False

directive :: String -> String -> Bool 
directive xs name = name `isPrefixOf` xs
