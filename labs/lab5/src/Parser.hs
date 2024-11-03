module Main where
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

main = do
  s <- getContents
  let ast = parseHasquelito (scanTokens s)
  print ast
  --print (run ast)
