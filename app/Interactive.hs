module Main where

import Orwell

import System.Environment
import System.Exit
import System.FilePath (takeExtension, hasExtension)
import System.IO
import System.Console.Haskeline
import Data.Foldable

printTypeEnv :: TypeEnv -> InputT IO ()
printTypeEnv ((bind, typ):tEnv) =
  outputStrLn $ bind ++ " : " ++ show typ

repl :: Config -> IO ()
repl conf = do
<<<<<<< HEAD
   env <- injectVariable "x" (Integer 3) IntType Nothing
   --env <- injectVariable "y" (Lambda "x" (Plus (Variable "x") (Variable "x")) (Arrow IntType IntType) (Arrow IntType IntType)) (Arrow IntType IntType) $ Just env
   
   loopREPL conf env
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
=======
  env <- injectVariable "x" (Integer 3) IntType Nothing 
  runInputT defaultSettings (loopREPL conf (env))
  where
    loopREPL :: Config -> (OpEnv, TypeEnv) -> InputT IO ()
    loopREPL conf topEnv = do
     mInput <- getInputLine "λ> "
     case mInput of
      Nothing      -> return () -- EOF / Ctrl-d
      Just ""      -> loopREPL conf topEnv
      Just ":q"    -> do 
        outputStrLn "Exiting REPL."
        return ()
      Just ":quit" -> do 
        outputStrLn "Exiting REPL." 
        return ()
      Just ":t"    -> do
        let (_, tEnv) = topEnv
        printTypeEnv tEnv
        loopREPL conf topEnv
      Just line    -> do
       --addHistory line
       let (out, env) = interp line topEnv conf
       forM_ out outputStrLn
       loopREPL conf env
>>>>>>> 7a73feb8b2adfb5c681340b25842d0b4c7560832


main = do
   putStrLn "Welcome to orwell REPL. Type ':q' to exit or 'Ctrl-D'."
   repl conf
   where
      conf = Config {
         shouldShowType = True
      }
