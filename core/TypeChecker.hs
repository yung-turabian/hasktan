module TypeChecker where
import Grammar
import Lexer

-- Type alias for type environments.

type TypeEnv = [(String,TypeExp)]

-- Function for retrieving types of variables form the environment.

lookup::String -> [(String,a)] -> a
lookup s  []                    = error ("Type " ++ s ++ " not defined in the current environment")
lookup s1 ((s2,t):l) | s1 == s2 = t
lookup s  (_:l)                 = TypeChecker.lookup s l



-- Please complete the definition of typeChecker for the rest of the abstract syntax.

typeChecker :: E AST -> TypeEnv -> TypeExp

-- Constants
typeChecker (Ok (Boolean _)) _ = BoolType
typeChecker (Ok (Integer _)) _ = IntType

-- *Additional types
typeChecker (Ok (Float _)) _   = FloatType

-- Variables
typeChecker (Ok (Variable s) ) env = TypeChecker.lookup s env

-- Operations
typeChecker (Ok(Plus e1 e2)) env =
   let t1 = typeChecker (Ok e1) env
       t2 = typeChecker (Ok e2) env
   in
   if t1 == IntType && t2 == IntType 
   then IntType 
   else if t1 == FloatType && t2 == FloatType
   then FloatType 
   else error $ "type mismatch:\n\t" ++ show t1 ++ "\n\t" ++ show t2


typeChecker (Ok(Minus e1 e2)) env =
   let t1 = typeChecker (Ok e1) env
       t2 = typeChecker (Ok e2) env
   in
   if t1 == IntType && t2 == IntType 
   then IntType 
   else if t1 == FloatType && t2 == FloatType
   then FloatType 
   else error $ "type mismatch:\n\t" ++ show t1 ++ "\n\t" ++ show t2

typeChecker (Ok(Times e1 e2)) env =
   let t1 = typeChecker (Ok e1) env
       t2 = typeChecker (Ok e2) env
   in
   if t1 == IntType && t2 == IntType 
   then IntType 
   else if t1 == FloatType && t2 == FloatType
   then FloatType 
   else error $ "type mismatch:\n\t" ++ show t1 ++ "\n\t" ++ show t2

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
  t2 = typeChecker (Ok e2) env



typeChecker (Ok(And e1 e2)) env 
 | t1 == BoolType && t2 == BoolType = BoolType
 where
  t1 = typeChecker (Ok e1) env
  t2 = typeChecker (Ok e2) env

typeChecker (Ok(Or e1 e2)) env 
 | t1 == BoolType && t2 == BoolType = BoolType
 where
  t1 = typeChecker (Ok e1) env
  t2 = typeChecker (Ok e2) env


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
   else error $ "Mismatch types `" ++ (show t1) ++ " == " ++ (show t2) ++ "`."



-- Let expressions
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


-- Lambda expressions
typeChecker (Ok(Lambda x e s t1)) env
 | t2 == t1 = (Arrow s t1)
 | otherwise = error ("Couldn't match expected: \n\t" ++ (show t1) ++ "\n With actual type: \n\t" ++ (show t2) )
 where
  t2 = typeChecker (Ok e) env
  env = (x, s) : env

-- Function application
typeChecker (Ok(App e1 e2)) env
 | s1 == s2 = t
 | otherwise = error $ show s1 ++ " != " ++ show s2
 where
  (Arrow s1 t) = typeChecker (Ok e1) env
  s2 = typeChecker(Ok e2) env

typeChecker (Ok(Not)) env = Arrow BoolType BoolType


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


typeChecker (Ok(Concat e1 e2)) env 
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

typeChecker (Ok(Head e)) env 
 | t == ListType IntType = IntType
 | t == ListType FloatType = FloatType
 | t == ListType BoolType = BoolType
 
 | otherwise = error "List head."
 where 
  t = typeChecker (Ok (e)) env

typeChecker (Ok(Tail e)) env 
 | t == ListType IntType = ListType IntType
 | t == ListType FloatType = ListType FloatType
 | t == ListType BoolType = ListType BoolType
 
 | otherwise = error "List tail."
 where 
  t = typeChecker (Ok (e)) env






typeChecker e _ = error $ "Unknown type: " ++ show e

-- http://www.zvon.org/other/haskell/Outputprelude/all_f.html
-- Rewrote `all`, just passes a condition to all children
hwAll :: (a -> Bool) -> [a] -> Bool
hwAll _ [] = True
hwAll cond (it:list) | cond it = hwAll cond list
hwAll cond (it:list) = False

len :: [a] -> Int
len [] = 0
len (_:list) = 1 + len list
