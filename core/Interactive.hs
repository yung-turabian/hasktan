module Main where
import System.Environment

import System.Exit
import System.FilePath (takeExtension, hasExtension)
import System.IO

-- External dependency
import System.Console.Readline

import Hasktan

exit = exitWith ExitSuccess
exitBad = exitWith (ExitFailure 1)

repl :: IO ()
repl = do
   maybeLine <- readline "prelude> "
   case maybeLine of
      Nothing -> return () -- EOF / Ctrl-d
      Just "" -> repl
      Just ":q" -> putStrLn "Exiting REPL." >> return ()

      Just line -> do addHistory line
                      interpretPrint line 
                      repl


main = do
    putStrLn "Welcome to Hasktan REPL. Type ':q' to exit or 'Ctrl-D'."
    repl 
