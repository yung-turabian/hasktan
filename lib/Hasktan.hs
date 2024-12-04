module Hasktan where
import Control.Exception

import TypeChecker
import Grammar
import Lexer

type OpEnv = [(String,AST)]

-- (remove_var x sigma) computes sigma_x.

remove_var :: String -> OpEnv -> OpEnv
remove_var x [] = []
remove_var x ((y,_):env) | x == y =
    remove_var x env
remove_var x ((y,ast):env) =
    (y,ast) : remove_var x env

-- Substitute an AST for a variable in an AST.

subst_var :: String -> AST -> AST -> AST
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
subst_var x e Not =
    (subst_var x e (Lambda "b" (If (Variable "b") (Boolean False) (Boolean True)) BoolType BoolType))
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
subst_var x e (List ast) =
    List (map (subst_var x e) ast)
subst_var x e (Cons ast1 ast2) =
    Cons (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Concat ast1 ast2) =
    Concat (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Head ast) =
    Head (subst_var x e ast)
subst_var x e (Tail ast) =
    Tail (subst_var x e ast)



-- Treat an environment as a variable substitution.
subst :: OpEnv -> AST -> AST
subst [] ast = ast
subst ((x,e):env) ast =
    subst env (subst_var x e ast)

interpreter:: E AST -> OpEnv -> E AST
interpreter (Ok(Boolean b)) _ = Ok(Boolean b)
interpreter (Ok(Integer n)) _ = Ok(Integer n)

-- Extra types
interpreter (Ok(List l)) _ = Ok(List l)
interpreter (Ok(Float f)) _ = Ok(Float f)

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

interpreter (Ok(Divide e1 e2)) env =
    let
       Ok (Float n1) = interpreter (Ok e1) env
       Ok (Float n2) = interpreter (Ok e2) env
    in 
      Ok (Float (n1 / n2))

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

-- Lambda expressions
interpreter (Ok(Lambda x e s t)) env =
 (Ok (Lambda x e s t))
 where
   env = remove_var x env

-- Let expressions
interpreter (Ok(Let x e1 e2)) env = 
 interpreter (Ok e2) env
 where
   Ok interpE1 = interpreter (Ok e1) env
   env = (x, interpE1) : env

interpreter (Ok(LetRec x e1 e2)) env =
   case e1 of
      Lambda y e s t ->
         let env' = (x, Lambda y (subst [(x, e1)] e) s t) : env
         in interpreter (Ok e2) env'
      _ -> error "LetRec only supports function definitions"


-- Function Application
interpreter (Ok(App e1 e2)) env = 
  interpreter (Ok(subst [(x, interpE2)] e)) env
   where
      Ok (Lambda x e _ _) = interpreter (Ok e1) env
      Ok interpE2 = interpreter (Ok e2) env

interpreter (Ok(Not)) env = Ok(Lambda "b" (If (Variable "b") (Boolean False) (Boolean True)) BoolType BoolType)



-- List manipulation
interpreter (Ok(Cons e1 e2)) env =
    let
       t1 = interpreter (Ok e1) env
       Ok (List l) = interpreter (Ok e2) env
    in case t1 of
      (Ok (Integer n)) -> Ok (List (Integer n : l))
      (Ok (Float n)) -> Ok (List (Float n : l))
      (Ok (Boolean n)) -> Ok (List (Boolean n : l))

interpreter (Ok(Concat e1 e2)) env =
    let
       Ok (List l1) = interpreter (Ok e1) env
       Ok (List l2) = interpreter (Ok e2) env
    in Ok (List (l1 ++ l2))


interpreter (Ok(Head e)) env =
   let
      Ok (List l) = interpreter (Ok e) env
   in
      Ok (head l)

interpreter (Ok(Tail e)) env =
   let
      Ok (List l) = interpreter (Ok e) env
   in
      Ok (List(tail l))

interpreter e _ = error ("Unable to interpret: " ++ show(e))

{- MAYBE LATER, implement type classes and types.
 -
 - class HasktanNum a where
   (+) :: a -> a -> a
   (-) :: a -> a -> a
   fromInteger :: Integer -> a

data HasktanType = 
     HaskInt Int 
   | HaskFloat Float 
   | HaskBool Bool 
   | HaskList [AST]
   deriving (Eq,Show)-}

getActualValue :: AST -> Int
getActualValue (Integer i) = i


-- Makes it easy to print lists all nice and pretty.
prettyPrint :: AST -> String
prettyPrint (Boolean b) = show b
prettyPrint (Integer n) = show n
prettyPrint (Float f) = show f
prettyPrint (List l) = "[" ++ prettyList l ++ "]"
 where
   prettyList [] = ""
   prettyList [l] = prettyPrint l
   prettyList (l:it) = prettyPrint l ++ ", " ++ prettyList it
prettyPrint _ = "Unsupported for pretty print :("


typeCheck :: String -> IO ()
typeCheck s = do
   let ast = parseHasquelito (scanTokens s)
   let t = typeChecker ast []
   print t 

interpretPrint :: String -> IO ()
interpretPrint s = do
    let handler :: SomeException -> IO Bool
        handler e = do
            putStrLn "Exception caught: "
            print e
            return False

    let ast = parseHasquelito (scanTokens s)
    let t = typeChecker ast []
    let Ok val = interpreter ast []

    parseResult <- ((evaluate (ast) >> return True) `catch` handler)
    if not parseResult then return () else do
      typeCheckResult <- (evaluate (t) >> return True) `catch` handler
      if not typeCheckResult then return () else do
         res <- (evaluate (val) >> return True) `catch` handler
         if not res
            then return ()
            else putStrLn (prettyPrint val)

interpret :: String -> Int
interpret s = do
    let ast = parseHasquelito (scanTokens s)
    let t = typeChecker ast []
    let Ok val = interpreter ast []
    getActualValue (val)
