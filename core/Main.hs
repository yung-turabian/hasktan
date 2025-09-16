module Main where
import System.Environment

import System.Exit
import System.FilePath (takeExtension, hasExtension)
import System.IO

-- External dependency
--import System.Console.Readline

import Hasqtan

usage = putStrLn "Usage: hasqtan <file.hs>"
version = putStrLn "hasqtan -- An interpreted language - 0.1"
exitBad = exitWith (ExitFailure 1)

{-repl :: IO ()
repl = do
   maybeLine <- readline "hasqtan> "
   case maybeLine of
      Nothing -> return () -- EOF / Ctrl-d
      Just "" -> repl
      Just ":q" -> putStrLn "Exiting REPL." >> return ()

      Just line -> do addHistory line
                      interpretPrint line 
                      repl
-}

main = do
    args <- getArgs
    ret <- case args of
      ["-h"] -> usage >> exitSuccess
      ["--version"] -> version >> exitSuccess
      ["-v"] -> version >> exitSuccess
      {-["-i"] -> do
                  putStrLn "Welcome to hasqtan REPL. Type ':q' to exit or 'Ctrl-D'."
                  repl-}
      ["-t", file] | hasExtension file -> do
         if takeExtension file == ".hq" 
         then do 
            contents <- readFile file
            typeCheckAndPrint contents
         else die "Please use a .hq file."
      [file] | hasExtension file -> do
         if takeExtension file == ".hq" 
         then do 
            contents <- readFile file
            putStrLn (interp contents)
         else die "Please use a .hq file."
      _ -> usage >> exitBad

    return ()
