module Main where
import Control.Monad (unless)
import System.IO
import Grammar
import Lexer


{-
type Env = String -> AST
emptyEnv = error "Not found"
envLookup s env = env s
envBind s v env = (\s' -> if s == s' then v else env s)

eval :: AST -> Env -> Int
eval (Integer v) _ = v
eval (Add e1 e2) env = (eval e1 env) + (eval e2 env)
eval (Sub e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Mul e1 e2) env = (eval e1 env) * (eval e2 env)
eval (Div e1 e2) env = (eval e1 env) `div` (eval e2 env)
eval (Negate e) env = -(eval e env)
eval (Variable s) env = eval (envLookup s env) env
eval (LetIn s e1 e2) env = eval e2 env'
 where env' = envBind s e1 env


run :: E AST -> Int
run (Ok e) = eval e emptyEnv
run (Failed err) = error ("Parsing failed: " ++ err)-}

repl :: IO ()
repl = do
 putStr "$ "
 input <- getLine
 unless( input == ":quit" || input == "exit") $ do
  print (parseHasquelito(scanTokens input))
  repl

main :: IO ()
main = do
 --hSetBuffering stdout NoBuffering
 --putStrLn "Welcome to the Hasktan REPL! Type :quit to leave."
 --repl

  s <- getContents
  let ast = parseHasquelito (scanTokens s)
  print ast 
