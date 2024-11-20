module Main where
import TypeChecker
import Grammar
import Lexer

type OpEnv = [(String,AST)]

-- (remove_var x sigma) computes sigma_x.

remove_var:: String -> OpEnv -> OpEnv
remove_var x [] = []
remove_var x ((y,_):env) | x == y =
    remove_var x env
remove_var x ((y,ast):env) =
    (y,ast) : remove_var x env

-- Substitute an AST for a variable in an AST.

subst_var:: String -> AST -> AST -> AST
subst_var _ _ (Boolean b) = Boolean b
subst_var _ _ (Integer n) = Integer n
subst_var x e (Variable v) | x == v = e
subst_var x e (Variable v) = Variable v
subst_var x e (Plus ast1 ast2) =
    Plus (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Minus ast1 ast2) =
    Minus (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Times ast1 ast2) =
    Times (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Rem ast1 ast2) =
    Rem (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Quot ast1 ast2) =
    Quot (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (And ast1 ast2) =
    And (subst_var x e ast1) (subst_var x e ast2)
--subst_var x e (Not ast) =
--    Not (subst_var x e ast)
subst_var x e (Or ast1 ast2) =
    Or (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Equals ast1 ast2) =
    Equals (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Lt ast1 ast2) =
    Lt (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Gt ast1 ast2) =
    Gt (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (App ast1 ast2) =
    App (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (If ast1 ast2 ast3) =
    If (subst_var x e ast1) (subst_var x e ast2) (subst_var x e ast3)
subst_var x e (Let y ast1 ast2) | y == x =
    Let y (subst_var x e ast1) ast2
subst_var x e (Let y ast1 ast2) =
    Let y (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Lambda y ast t1 t2) | x == y =
    Lambda y ast t1 t2
subst_var x e (Lambda y ast t1 t2) =
    Lambda y (subst_var x e ast) t1 t2

-- Treat an environment as a variable substitution.

subst:: OpEnv -> AST -> AST
subst [] ast = ast
subst ((x,e):env) ast =
    subst env (subst_var x e ast)

-- Please complete the definition of interpreter.

interpreter:: E AST -> OpEnv -> E AST
interpreter (Ok(Boolean b)) _ = Ok (Boolean b)
interpreter (Ok(Integer n)) _ = Ok (Integer n)
interpreter (Ok(Float f)) _ = Ok (Float f)
interpreter (Ok(Variable v)) env =
    let
        e = TypeChecker.lookup v env
    in
        interpreter (Ok e) env

interpreter (Ok(Plus e1 e2)) env =
    let
       Ok t1 = interpreter (Ok e1) env
       Ok t2 = interpreter (Ok e2) env
    in case (t1, t2) of
      (Integer n1, Integer n2) -> Ok (Integer (n1 + n2))
      (Float n1, Float n2) -> Ok (Float (n1 + n2))
      _ -> Failed "Type mismatch."

interpreter (Ok(Minus e1 e2)) env =
    let
       Ok t1 = interpreter (Ok e1) env
       Ok t2 = interpreter (Ok e2) env
    in case (t1, t2) of
      (Integer n1, Integer n2) -> Ok (Integer (n1 - n2))
      (Float n1, Float n2) -> Ok (Float (n1 - n2))

interpreter (Ok(Times e1 e2)) env =
    let
       Ok t1 = interpreter (Ok e1) env
       Ok t2 = interpreter (Ok e2) env
    in case (t1, t2) of
      (Integer n1, Integer n2) -> Ok (Integer (n1 * n2))
      (Float n1, Float n2) -> Ok (Float (n1 * n2))

interpreter (Ok(Power e1 e2)) env =
    let
       Ok t1 = interpreter (Ok e1) env
       Ok t2 = interpreter (Ok e2) env
    in case (t1, t2) of
      (Integer n1, Integer n2) -> Ok (Integer (n1 ^ n2))
      (Float n1, Integer n2) -> Ok (Float (n1 ^ n2))

interpreter (Ok(Quot e1 e2)) env =
    let
       (Ok(Integer n1)) = interpreter (Ok e1) env
       (Ok(Integer n2)) = interpreter (Ok e2) env
    in 
      Ok(Integer(quot n1 n2))


interpreter (Ok(Rem e1 e2)) env =
    let
       (Ok(Integer n1)) = interpreter (Ok e1) env
       (Ok(Integer n2)) = interpreter (Ok e2) env
    in 
      Ok(Integer(rem n1 n2))


interpreter (Ok(And e _)) env
 | b == False = (Ok(Boolean False))
 where
   (Ok (Boolean b)) = interpreter (Ok e) env

interpreter (Ok(And e1 e2)) env
 | (b1 == True) = (Ok(Boolean b2))
 where
   (Ok(Boolean b1)) = interpreter (Ok e1) env
   (Ok(Boolean b2)) = interpreter (Ok e2) env

interpreter (Ok(Or e _)) env
 | b == True = (Ok(Boolean True))
 where
   (Ok (Boolean b)) = interpreter (Ok e) env

interpreter (Ok(Or e1 e2)) env
 | (b1 == False) = (Ok(Boolean b2))
 where
   (Ok(Boolean b1)) = interpreter (Ok e1) env
   (Ok(Boolean b2)) = interpreter (Ok e2) env


-- Predicates

interpreter (Ok(Equals e1 e2)) env
 | v1 == v2 = Ok(Boolean(True))
 where
   Ok v1 = interpreter (Ok e1) env
   Ok v2 = interpreter (Ok e2) env

interpreter (Ok(Equals e1 e2)) env
 | v1 /= v2 = Ok(Boolean(False))
 where
   Ok v1 = interpreter (Ok e1) env
   Ok v2 = interpreter (Ok e2) env


interpreter (Ok(Gt e1 e2)) env
 | v1 > v2 = Ok(Boolean(True))
 where
   Ok v1 = interpreter (Ok e1) env
   Ok v2 = interpreter (Ok e2) env

interpreter (Ok(Gt e1 e2)) env
 | v1 <= v2 = Ok(Boolean(False))
 where
   Ok v1 = interpreter (Ok e1) env
   Ok v2 = interpreter (Ok e2) env

interpreter (Ok(Lt e1 e2)) env
 | v1 < v2 = Ok(Boolean(True))
 where
   Ok v1 = interpreter (Ok e1) env
   Ok v2 = interpreter (Ok e2) env

interpreter (Ok(Gt e1 e2)) env
 | v1 >= v2 = Ok(Boolean(False))
 where
   Ok v1 = interpreter (Ok e1) env
   Ok v2 = interpreter (Ok e2) env


-- If expressions
interpreter (Ok(If e1 e2 e3)) env
 | interpreter (Ok e1) env == Ok(Boolean(True)) = Ok v
 where
   Ok v = interpreter (Ok e2) env

interpreter (Ok(If e1 e2 e3)) env
 | interpreter (Ok e1) env == Ok(Boolean(False)) = Ok v
 where
   Ok v = interpreter (Ok e3) env


-- Let expressions
-- interpreter (Ok(Let x e1 e2)) env =


main = do
  s <- getContents
  let ast = parseHasquelito (scanTokens s)
  let t = typeChecker ast []
  let val = interpreter ast []
  print (ast,t,val)  
