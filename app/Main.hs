module Main where

import Reva

import System.Environment
import System.Exit
import System.FilePath (takeExtension, hasExtension)
import System.IO

usage = putStrLn "Usage: reva <file.hs>"
version = putStrLn "reva -- An interpreted language - 0.0.1"
exitBad = exitWith (ExitFailure 1)

main = do
    args <- getArgs
    ret <- case args of
      ["-h"] -> usage >> exitSuccess
      ["--version"] -> version >> exitSuccess
      ["-v"] -> version >> exitSuccess
      ["-t", file] | hasExtension file -> do
         if takeExtension file == ".reva" 
         then do 
            contents <- readFile file
            typeCheckAndPrint contents
         else die "Please use a .reva file."
      [file] | hasExtension file -> do
         if takeExtension file == ".reva" 
         then do 
            contents <- readFile file
            case interp CMD contents ([],[]) conf of
               (Just str, _) -> putStrLn str
               (Nothing, _) -> return ()
         else die "Please use a .reva file."
      _ -> usage >> exitBad

    return ()
   where
      conf = Config {
         shouldShowType = True
      }
