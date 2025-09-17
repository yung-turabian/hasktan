module Main where

import Hasqtan

import System.Environment
import System.Exit
import System.FilePath (takeExtension, hasExtension)
import System.IO
import System.Console.Readline
import Data.IORef
import Data.Foldable

repl :: Config -> IO ()
repl conf =
   let env = injectVariable "x" (Integer 3) IntType Nothing
   in
   loopREPL conf (env)
   where
      loopREPL :: Config -> (OpEnv, TypeEnv) -> IO ()
      loopREPL conf topEnv = do
         maybeLine <- readline "λ> "
         case maybeLine of
            Nothing -> exitSuccess -- EOF / Ctrl-d
            Just "" -> loopREPL conf topEnv
            Just ":q" -> putStrLn "Exiting REPL." >> exitSuccess
            Just ":exit" -> putStrLn "Exiting REPL." >> exitSuccess
            Just line -> do
               addHistory line
               let (out, env) = interp line topEnv conf
               forM_ out putStrLn
               loopREPL conf env


main = do
   putStrLn "Welcome to hasqtan REPL. Type ':q' to exit or 'Ctrl-D'."
   topEnv <- newIORef []
   repl conf
   where
      conf = Config {
         shouldShowType = True
      }
