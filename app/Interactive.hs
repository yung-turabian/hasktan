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
  contents <- readFile "./Prelude.orwell"
  let (mOut, env) = interp CMD contents ([],[]) conf
  {-case mOut of
    Just out -> putStrLn out
    Nothing -> return ()-}
  --env <- injectVariable "x" (Integer 3) IntType Nothing 
  --env <- injectVariable "y" (Lambda "x" (Plus (Variable "x") (Variable "x")) (Arrow IntType IntType) (Arrow IntType IntType)) (Arrow IntType IntType) $ Just env
  runInputT defaultSettings (loopREPL conf env)
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
       let (out, env) = interp REPL line topEnv conf
       forM_ out outputStrLn
       loopREPL conf env

main = do
   putStrLn "Welcome to orwell REPL. Type ':q' to exit or 'Ctrl-D'."
   repl conf
   where
      conf = Config {
         shouldShowType = True
      }
