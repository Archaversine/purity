module Config where 

import System.FilePath

mainColor :: String 
mainColor = "\ESC[35m"

resetColor :: String 
resetColor = "\ESC[0m"

boldColor :: String 
boldColor = "\ESC[1m"

-- Required by Shell
splashText :: Maybe String 
splashText = Just $ mainColor <> "Purity" <> resetColor <> " - A Lazily Evaluated Purely Functional Terminal\n"

formatDir :: FilePath -> String
formatDir "/" = "/" 
formatDir x = takeBaseName x

-- Required by Shell
commandPrompt :: FilePath -> String 
commandPrompt path = mconcat [ " ("
                             , mainColor
                             , "λ ", resetColor, boldColor
                             , formatDir path
                             , resetColor
                             , ") "
                             ]

-- Required by Shell
codePrompt :: FilePath -> String 
codePrompt _ = mainColor <> " λ " <> resetColor

-- Required by Shell
blockPrompt :: Maybe String 
blockPrompt = Just "| "

-- Required by Shell
defaultImports :: [String] 
defaultImports = [ "Prelude"
                 , "System.Directory"
                 ]

-- Required by Shell
errorColorPrefix :: Maybe String 
errorColorPrefix = Just "\ESC[38;5;161m"
