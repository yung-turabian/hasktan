module Orwell.TypeChecker (
   lookup,
   typeCheck
) where

import Orwell.Grammar
import Orwell.Lexer
import Orwell.Util

import Control.Monad.State

type Typechecker a = State TypeEnv a

lookupVar :: String -> Typechecker (Maybe TypeExp)
lookupVar x = do gets (lookup x)

setVar :: String -> TypeExp -> Typechecker ()
setVar x t = do
    env <- get
    let newEnv = (x, t) : env
    put newEnv

resetEnv :: Typechecker ()
resetEnv = put []

-- | Helper for binary arithmetic ops
checkBinOp :: (AST,  AST) -> Typechecker (E TypeExp)
checkBinOp (l, r) = do
   t1 <- typeChecker $ Ok l
   t2 <- typeChecker $ Ok r
   case (t1, t2) of
      (Ok IntType, Ok IntType)     -> return $ Ok IntType
      (Ok FloatType, Ok FloatType) -> return $ Ok FloatType
      _ -> return $ Failed ("type mismatch:\n\t" ++ show t1 ++ "\n\t" ++ show t2)

-- TODO we need a monad state to track environments types too.
typeChecker :: E AST -> Typechecker (E TypeExp)
typeChecker (Ok ast) =
    case ast of
         Boolean b   -> return $ Ok BoolType
         Integer n   -> return $ Ok IntType
         Float f     -> return $ Ok FloatType

         Variable v -> do
            maybeType <- lookupVar v
            case maybeType of
                  Just t -> return $ Ok t
                  Nothing  -> return $ Failed (formatErrorCode NotMemeberOfEnvironment $ "Type `" ++ v ++ "` is not a member of this environment")


         -- Binary Operations
         Plus e1 e2  -> checkBinOp (e1, e2)
         Minus e1 e2 -> checkBinOp (e1, e2)
         Times e1 e2 -> checkBinOp (e1, e2)
         Power e1 e2 -> checkBinOp (e1, e2)

         Binding x _ _ v -> do
               t_v <- typeChecker $ Ok v
               case t_v of
                  Ok t -> do
                     setVar x t
                     return $ Ok VoidType
                  Failed msg ->
                     return (Failed msg)

         -- Function application
         App e1 e2 -> do
            t_func <- typeChecker $ Ok e1
            case t_func of
               Ok (Arrow s1 t_sig) -> do
                  t_app <- typeChecker $ Ok e2
                  case t_app of
                     Ok t_ret -> do
                        if t_sig == t_ret
                           then return $ Ok t_ret
                           else return $ Failed (show t_sig ++ " != " ++ show t_ret)
                     Failed msg ->
                        return (Failed msg)
               Failed msg ->
                     return (Failed msg)

         --typeChecker (Ok(Not)) env = Arrow BoolType BoolType

         e -> return $ Failed ("[HSQ-TypeChecker-UNCAUGHT] " ++ show e)

{--- Variables
-}

{-

-- Returns a float no matter what
typeChecker (Ok(Divide e1 e2)) env 
 | t1 == FloatType && t2 == FloatType = FloatType 
 | otherwise = error "Only supports use with Floats."
 where
  t1 = typeChecker (Ok e1) env
  t2 = typeChecker (Ok e2) env

-- Operand can't be a float
typeChecker (Ok(Power e1 e2)) env 
 | (t1 == IntType || t1 == FloatType) && (t2 == IntType) = t1
 | otherwise = error ("Type mismatch between: \n\t" ++ (show t1) ++ "\nAnd type: \n\t" ++ (show t2) ++ "\nIn equation")
 where
  t1 = typeChecker (Ok e1) env
  t2 = typeChecker (Ok e2) env


typeChecker (Ok(Quot e1 e2)) env 
 | t1 == IntType && t2 == IntType = IntType 
 | otherwise = error "Only supports use with Integers."
 where
  t1 = typeChecker (Ok e1) env
  t2 = typeChecker (Ok e2) env

typeChecker (Ok(Rem e1 e2)) env 
 | t1 == IntType && t2 == IntType = IntType 
 | otherwise = error "Only supports use with Integers."
 where
  t1 = typeChecker (Ok e1) env
  t2 = typeChecker (Ok e2) env-}



{-typeChecker (Ok(And e1 e2)) env
 | t1 == Ok BoolType && t2 == Ok BoolType = Ok BoolType
 where
  t1 = typeChecker (Ok e1) env
  t2 = typeChecker (Ok e2) env

typeChecker (Ok(Or e1 e2)) env
 | t1 == Ok BoolType && t2 == Ok BoolType = Ok BoolType
 where
  t1 = typeChecker (Ok e1) env
  t2 = typeChecker (Ok e2) env
-}
{-
-- Predicates
typeChecker (Ok(Equals e1 e2)) env =
 let t1 = typeChecker (Ok (e1)) env
     t2 = typeChecker (Ok (e2)) env
 in
   if t1 == t2
   then BoolType
   else error $ "Mismatch types `" ++ (show t1) ++ " == " ++ (show t2) ++ "`."


typeChecker (Ok(Gt e1 e2)) env =
 let t1 = typeChecker (Ok (e1)) env
     t2 = typeChecker (Ok (e2)) env
 in
   if t1 == t2
   then BoolType
   else error $ "Mismatch types `" ++ (show t1) ++ " == " ++ (show t2) ++ "`."

typeChecker (Ok(Lt e1 e2)) env =
 let t1 = typeChecker (Ok (e1)) env
     t2 = typeChecker (Ok (e2)) env
 in
   if t1 == t2
   then BoolType
   else error $ "Mismatch types `" ++ (show t1) ++ " == " ++ (show t2) ++ "`."-}

{-typeChecker (Ok (Binding x t x2 body)) env = do
   if x /= x2 then
      error "Binding names don't match"
   else
      typeChecker (Ok body) env
-}

{--- Let expressions
typeChecker (Ok(Let x (Lambda _ _ _ _) e2)) env =
   error $ "\x1b[1;31mUse `letrec` for recursion via anonymous function.\x1b[0;0m" 
typeChecker (Ok(Let x e1 e2)) env = 
   let s = typeChecker (Ok e1) env
       env = (x, s) : env
   in
      if typeChecker (Ok (Variable x)) env == s 
      then typeChecker (Ok e2) env
      else error $ "\x1b[1;31mType mismatch in let: expected " ++ show s ++ ", got " ++ show (typeChecker (Ok e2) env) ++ "\x1b[0;0m"

-- Recursive Let expressions
-- letrec only supports function definitions
typeChecker (Ok(LetRec x (Lambda y body s t) e2)) env =
   let env = (x, (Arrow s t)) : env
   in if typeChecker (Ok body) ((y, s) : env) == t
      then typeChecker (Ok e2) env
      else error $ "Type mismatch in function"

-- If expressions
typeChecker (Ok(If e1 e2 e3)) env =
   let t1 = typeChecker (Ok e1) env
       t2 = typeChecker (Ok e2) env
       t3 = typeChecker (Ok e3) env
   in
      if t1 == BoolType && t2 == t3
      then t2
      else error $ "\x1b[1;31mCheck your if...then...else formatting.\x1b[0;0m"
-}

-- Lambda expressions
{-typeChecker (Ok(Lambda x e s t1)) env
 | t2 == Ok t1 = Ok $ Arrow s t1
 | otherwise = error ("Couldn't match expected: \n\t" ++ show t1 ++ "\n With actual type: \n\t" ++ show t2 )
 where
  t2 = typeChecker (Ok e) env
  env = (x, s) : env
-}
{-


-- List manipulation
typeChecker (Ok(List e)) env
 | len types == 0 = EmptyList
 | hwAll (== IntType) types = ListType IntType
 | hwAll (== BoolType) types = ListType BoolType
 | hwAll (== FloatType) types = ListType FloatType
 | otherwise = error "List of mixed types is not allowed"
 where
  types = map (\e -> typeChecker (Ok e) env) e


typeChecker (Ok(Cons e1 e2)) env
 | t1 == IntType && (t2 == ListType IntType || t2 == EmptyList) = ListType IntType
 | t1 == FloatType && (t2 == ListType FloatType || t2 == EmptyList) = ListType FloatType
 | t1 == BoolType && (t2 == ListType BoolType || t2 == EmptyList) = ListType BoolType
 | otherwise = error "List construction type mismatch."
 where 
  t1 = typeChecker (Ok (e1)) env
  t2 = typeChecker (Ok(e2)) env


typeChecker (Ok(Concat e1 e2)) env = do
 | t1 == EmptyList && t2 == EmptyList = EmptyList
 | (t1 == BoolType || t1 == EmptyList) && (t2 == BoolType || t2 == EmptyList) = ListType BoolType
 | (t1 == ListType IntType || t1 == EmptyList) && (t2 == ListType IntType || t2 == EmptyList) = ListType IntType
 | t1 == ListType FloatType && (t2 == ListType IntType || t2 == ListType FloatType || t2 == EmptyList) = ListType FloatType
 | t1 == ListType IntType && t2 == ListType FloatType = ListType FloatType
 | t1 == EmptyList && t2 == ListType FloatType = ListType FloatType
 | t1 == EmptyList && t2 == ListType IntType = ListType IntType
 
 | otherwise = error "List concatenation type mismatch."
 where 
  t1 = typeChecker (Ok (e1)) env
  t2 = typeChecker (Ok (e2)) env

typeChecker (Ok (Head e)) env = do
  t <- typeChecker (Ok e) env
  case t of
    ListType IntType   -> Right (ListType IntType)
    ListType FloatType -> Right (ListType FloatType)
    ListType BoolType  -> Right (ListType BoolType)
    _                  -> Left "List head: invalid type"

typeChecker (Ok (Tail e)) env = do
  t <- typeChecker (Ok e) env
  case t of
    ListType IntType   -> Right (ListType IntType)
    ListType FloatType -> Right (ListType FloatType)
    ListType BoolType  -> Right (ListType BoolType)
    _                  -> Left "List tail: invalid type"-}   

--typeChecker (Failed errMsg) = return Failed ("error: [HSQ-" ++ errMsg)


-- | Runs the type checker
typeCheck :: E AST -> TypeEnv -> (E TypeExp, TypeEnv)
typeCheck ast = runState (typeChecker ast)
