module Config where 

import System.FilePath

mainColor :: String 
mainColor = "\ESC[35m"

resetColor :: String 
resetColor = "\ESC[0m"

boldColor :: String 
boldColor = "\ESC[1m"

splashText :: Maybe String 
splashText = Just $ mainColor <> "Purity" <> resetColor <> " - A Lazily Evaluated Purely Functional Terminal\n"

commandPrompt :: FilePath -> String 
commandPrompt path = mconcat [ mainColor 
                             , " λ "
                             , resetColor, boldColor 
                             , "(" <> takeBaseName path <> ")"
                             , resetColor 
                             , " "
                             ]

codePrompt :: FilePath -> String 
codePrompt _ = mainColor <> " λ" <> resetColor <> "= "

blockPrompt :: Maybe String 
blockPrompt = Just "| "

defaultImports :: [String] 
defaultImports = [ "Prelude"
                 , "System.Directory"
                 ]

errorColorPrefix :: Maybe String 
errorColorPrefix = Just "\ESC[38;5;161m"
