module Config where 

mainColor :: String 
mainColor = "\ESC[35m"

resetColor :: String 
resetColor = "\ESC[0m"

splashText :: Maybe String 
splashText = Just $ mainColor <> "Purity" <> resetColor <> " - A Lazily Evaluated Purely Functional Terminal\n"

shellPrompt :: String 
shellPrompt = mainColor <> " λ " <> resetColor

defaultImports :: [String] 
defaultImports = [ "Prelude"
                 ]

errorColorPrefix :: Maybe String 
errorColorPrefix = Just "\ESC[38;5;196m"
