module Reva.Interpreter where

import qualified Reva.TypeChecker as TC
import Reva.Lexer
import Reva.Util

import Control.Monad.State

type Interpreter a = State OpEnv a

lookupVar :: String -> Interpreter (Maybe AST)
lookupVar x = do gets (lookup x)

setVar :: String -> AST -> Interpreter ()
setVar x val = do
    env <- get
    let newEnv = (x, val) : removeVar x env
    put newEnv

resetEnv :: Interpreter ()
resetEnv = put []

-- (removeVar x sigma) computes sigma_x.
removeVar :: String -> OpEnv -> OpEnv
removeVar x [] = []
removeVar x ((y,_):env) | x == y =
    removeVar x env
removeVar x ((y,ast):env) =
    (y,ast) : removeVar x env

-- | Substitute an AST for a variable in an AST.
substVar :: String -> AST -> AST -> AST
substVar _ _ (Boolean b) = Boolean b
substVar _ _ (Integer n) = Integer n
substVar x e (Variable v) | x == v = e
substVar x e (Variable v) = Variable v
substVar x e (Plus ast1 ast2) =
    Plus (substVar x e ast1) (substVar x e ast2)
substVar x e (Minus ast1 ast2) =
    Minus (substVar x e ast1) (substVar x e ast2)
substVar x e (Times ast1 ast2) =
    Times (substVar x e ast1) (substVar x e ast2)
substVar x e (And ast1 ast2) =
    And (substVar x e ast1) (substVar x e ast2)
substVar x e Not =
    substVar x e (Lambda "b" (If (Variable "b") (Boolean False) (Boolean True)) BoolType BoolType)
substVar x e (Or ast1 ast2) =
    Or (substVar x e ast1) (substVar x e ast2)
substVar x e (Equals ast1 ast2) =
    Equals (substVar x e ast1) (substVar x e ast2)
substVar x e (Lt ast1 ast2) =
    Lt (substVar x e ast1) (substVar x e ast2)
substVar x e (Gt ast1 ast2) =
    Gt (substVar x e ast1) (substVar x e ast2)
substVar x e (App ast1 ast2) =
    App (substVar x e ast1) (substVar x e ast2)
substVar x e (If ast1 ast2 ast3) =
    If (substVar x e ast1) (substVar x e ast2) (substVar x e ast3)
substVar x e (Let y ast1 ast2) | y == x =
    Let y (substVar x e ast1) ast2
substVar x e (Let y ast1 ast2) =
    Let y (substVar x e ast1) (substVar x e ast2)
substVar x e (Lambda y ast t1 t2) | x == y =
    Lambda y ast t1 t2
substVar x e (Lambda y ast t1 t2) =
    Lambda y (substVar x e ast) t1 t2
substVar x e (List ast) =
    List (map (substVar x e) ast)
substVar x e (Cons ast1 ast2) =
    Cons (substVar x e ast1) (substVar x e ast2)
substVar x e (Concat ast1 ast2) =
    Concat (substVar x e ast1) (substVar x e ast2)
substVar x e (Head ast) =
    Head (substVar x e ast)
substVar x e (Tail ast) =
    Tail (substVar x e ast)

-- | NOTE: Treat an environment as a variable substitution.
subst :: OpEnv -> AST -> AST
subst [] ast = ast
subst ((x,e):env) ast =
    subst env (substVar x e ast)

interpBiOp :: (AST, AST) -> (Int -> Int -> Int) -> Interpreter (E AST)
interpBiOp (v1, v2) op = do
    ev1 <- interpreter $ Ok v1
    ev2 <- interpreter $ Ok v2
    case (ev1, ev2) of
        (Ok (Integer i1), Ok (Integer i2)) ->
            return $ Ok (Integer (op i1 i2))

interpDecls :: [AST] -> Interpreter (E AST)
interpDecls [] = return $ Ok EOL
interpDecls (decl:decls) = do 
  interpreter $ Ok decl
  interpDecls decls

interpreter :: E AST -> Interpreter (E AST)
{-interpreter (Ok(Quot e1 e2)) env =
    let
       (Ok(Integer n1)) = interpreter (Ok e1) env
       (Ok(Integer n2)) = interpreter (Ok e2) env
    in
      Ok (Integer (quot n1 n2))


interpreter (Ok(Rem e1 e2)) env =
    let
       (Ok(Integer n1)) = interpreter (Ok e1) env
       (Ok(Integer n2)) = interpreter (Ok e2) env
    in
      Ok (Integer (rem n1 n2))-}

{-interpreter (Ok(Or e _)) env
 | b == True = (Ok (Boolean True))
 where
   (Ok (Boolean b)) = interpreter (Ok e) env

interpreter (Ok(Or e1 e2)) env
 | (b1 == False) = (Ok (Boolean b2))
 where
   (Ok(Boolean b1)) = interpreter (Ok e1) env
   (Ok(Boolean b2)) = interpreter (Ok e2) env


-- Predicates

interpreter (Ok(Equals e1 e2)) env
 | v1 == v2 = Ok (Boolean (True))
 where
   Ok v1 = interpreter (Ok e1) env
   Ok v2 = interpreter (Ok e2) env

interpreter (Ok(Equals e1 e2)) env
 | v1 /= v2 = Ok (Boolean (False))
 where
   Ok v1 = interpreter (Ok e1) env
   Ok v2 = interpreter (Ok e2) env


interpreter (Ok(Gt e1 e2)) env
 | v1 > v2 = Ok (Boolean (True))
 where
   Ok v1 = interpreter (Ok e1) env
   Ok v2 = interpreter (Ok e2) env

interpreter (Ok(Gt e1 e2)) env
 | v1 <= v2 = Ok (Boolean (False))
 where
   Ok v1 = interpreter (Ok e1) env
   Ok v2 = interpreter (Ok e2) env

interpreter (Ok(Lt e1 e2)) env
 | v1 < v2 = Ok (Boolean (True))
 where
   Ok v1 = interpreter (Ok e1) env
   Ok v2 = interpreter (Ok e2) env

interpreter (Ok(Gt e1 e2)) env
 | v1 >= v2 = Ok (Boolean (False))
 where
   Ok v1 = interpreter (Ok e1) env
   Ok v2 = interpreter (Ok e2) env


-- If expressions
interpreter (Ok(If e1 e2 e3)) env
 | interpreter (Ok e1) env == Ok (Boolean True) = Ok v
 where
   Ok v = interpreter (Ok e2) env

interpreter (Ok(If e1 e2 e3)) env
 | interpreter (Ok e1) env == Ok (Boolean False) = Ok v
 where
   Ok v = interpreter (Ok e3) env

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
      Ok (List (tail l))-}

interpreter (Ok EOL) = return $ Ok EOL

-- TODO New, cleaner function. Still goes through above first
interpreter (Ok ast) =
    case ast of
        Program dls mE -> 
          case mE of 
            Just e -> do
              interpDecls dls
              interpreter $ Ok e
            Nothing ->
              interpDecls dls

        Boolean b  -> return $ Ok (Boolean b)
        Integer n  -> return $ Ok (Integer n)
        List l     -> return $ Ok (List l)
        Float f    -> return $ Ok (Float f)

        Variable v -> do
            maybeVal <- lookupVar v
            case maybeVal of
                Just val -> return $ Ok val
                Nothing  -> return $ Failed ("Unbound variable: " ++ v)

        Plus v1 v2 -> interpBiOp (v1, v2) (+)
        Minus v1 v2 -> interpBiOp (v1, v2) (-)
        Times v1 v2 -> interpBiOp (v1, v2) (*)
        Power v1 v2 -> interpBiOp (v1, v2) (^)


        And e1 e2 -> do
            result <- interpreter $ Ok e1
            case result of
                Ok (Boolean b1) ->
                    if not b1
                        then return $ Ok (Boolean False)
                        else interpreter (Ok e2)
                err -> return err

        Binding var _ v -> do
            var_val <- interpreter (Ok v)
            case var_val of
                Ok val -> do
                    setVar var val
                    return $ Ok EOL
                Failed msg ->
                    return (Failed msg)

        -- Lambda expressions, first-class value
        Lambda var body paramType retType ->
          return $ Ok (Lambda var body paramType retType)

        -- Function Application
        App func arg -> do
            func_val <- interpreter $ Ok func
            case func_val of
                Ok (Lambda var body _ _) -> do
                    arg_val <- interpreter $ Ok arg
                    case arg_val of
                        Ok val -> do
                            interpreter $ Ok (subst [(var, val)] body)


        e -> return $ Failed ("[HSQ-Interp-uncaught] " ++ show e)

interpreter (Failed errMsg) = return $ Failed ("error: [HSQ-" ++ errMsg)


runInterpreter :: E AST -> OpEnv -> (E AST, OpEnv)
runInterpreter ast = runState (interpreter ast)

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
