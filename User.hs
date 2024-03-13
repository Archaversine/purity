module User where 

import System.Exit

-- Basic example of config function
echo :: String -> IO ()
echo = putStrLn
    
exit :: IO ()
exit = exitWith ExitSuccess
