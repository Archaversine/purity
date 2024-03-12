module Config where 

mainColor :: String 
mainColor = "\ESC[35m"

resetColor :: String 
resetColor = "\ESC[0m"

splashText :: Maybe String 
splashText = Just $ mainColor <> "Purity" <> resetColor <> " - A Lazily Evaluated Purely Functional Terminal\n"

commandPrompt :: String 
commandPrompt = mainColor <> " λ " <> resetColor

codePrompt :: String 
codePrompt = mainColor <> " λ" <> resetColor <> "= "

blockPrompt :: Maybe String 
blockPrompt = Just "| "

defaultImports :: [String] 
defaultImports = [ "Prelude"
                 ]

errorColorPrefix :: Maybe String 
errorColorPrefix = Just "\ESC[38;5;161m"
