module Main where
import System.Environment

import System.Exit
import System.FilePath (takeExtension, hasExtension)
import System.IO

-- External dependency
import System.Console.Readline

import Hasktan

usage = putStrLn "Usage: hasktan <file.hs>"
version = putStrLn "Hasktan -- An interpreted language - 0.1"
exit = exitWith ExitSuccess
exitBad = exitWith (ExitFailure 1)


repl :: IO ()
repl = do
   maybeLine <- readline "🌿> "
   case maybeLine of
      Nothing -> return () -- EOF / Ctrl-d
      Just "" -> repl
      Just ":q" -> putStrLn "Exiting REPL." >> return ()

      Just line -> do addHistory line
                      interpretPrint line 
                      repl


main = do
    args <- getArgs
    ret <- case args of
      ["-h"] -> usage >> exit 
      ["--version"] -> version >> exit
      ["-v"] -> version >> exit
      ["-i"] -> do
                  putStrLn "Welcome to Hasktan REPL. Type ':q' to exit or 'Ctrl-D'."
                  repl 
      ["-t", file] | hasExtension file -> do
         if takeExtension file == ".hs" 
         then do 
            contents <- readFile file
            typeCheck contents
         else die "Please use a .hs file."
      [file] | hasExtension file -> do
         if takeExtension file == ".hs" 
         then do 
            contents <- readFile file
            interpretPrint contents
         else die "Please use a .hs file."
      _ -> usage >> exitBad


    

    return ()

