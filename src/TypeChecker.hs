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
typeChecker (Ok (Boolean _) ) _ = BoolType
typeChecker (Ok (Integer _) ) _ = IntType
typeChecker (Ok (Float _) ) _   = FloatType

-- Variables
typeChecker (Ok (Variable s) ) env = TypeChecker.lookup s env

-- Operations
typeChecker (Ok(Plus e1 e2)) env 
 | t1 == IntType && t2 == IntType = IntType 
 | t1 == FloatType && t2 == FloatType = FloatType 
 | otherwise = error ("type mismatch:\n\t" ++(show t1) ++ "\n\t" ++ (show t2))
 where
  t1 = typeChecker (Ok e1) env
  t2 = typeChecker (Ok e2) env

typeChecker (Ok(Minus e1 e2)) env 
 | t1 == IntType && t2 == IntType = IntType 
 | t1 == FloatType && t2 == FloatType = FloatType
 where
  t1 = typeChecker (Ok e1) env
  t2 = typeChecker (Ok e2) env

typeChecker (Ok(Times e1 e2)) env 
 | t1 == IntType && t2 == IntType = IntType 
 | t1 == FloatType && t2 == FloatType = FloatType
 where
  t1 = typeChecker (Ok e1) env
  t2 = typeChecker (Ok e2) env

-- Returns a float no matter what
typeChecker (Ok(Divide e1 e2)) env 
 | t1 == FloatType && t2 == FloatType = FloatType 
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
typeChecker (Ok(Equals e1 e2)) env
 | t1 == t2 = BoolType
 | otherwise = error ("Mismatch types at `" ++ (show t1) ++ " == " ++ (show t2) ++ "`.")
 where
  t1 = typeChecker (Ok (e1)) env
  t2 = typeChecker (Ok (e2)) env

typeChecker (Ok(Gt e1 e2)) env
 | t1 == t2 = BoolType
 | otherwise = error "Mismatch types >." 
 where
  t1 = typeChecker (Ok (e1)) env
  t2 = typeChecker (Ok (e2)) env

typeChecker (Ok(Lt e1 e2)) env
 | t1 == t2 = BoolType
 | otherwise = error "Mismatch types <." 
 where
  t1 = typeChecker (Ok (e1)) env
  t2 = typeChecker (Ok (e2)) env



-- Let expressions
typeChecker (Ok(Let x e1 e2)) env 
 | typeChecker (Ok (Variable x)) env == s = typeChecker (Ok e2) env
 where
  s = typeChecker (Ok e1) env
  env = (x, s) : env

-- If expressions
typeChecker (Ok(If e1 e2 e3)) env
 | t1 == BoolType && t2 == t3 = t2
 | otherwise = error "Check your IfThenElse formatting."
 where
  t1 = typeChecker (Ok e1) env
  t2 = typeChecker (Ok e2) env
  t3 = typeChecker (Ok e3) env

-- Lambda expressions
typeChecker (Ok(Lambda x e s t1)) env
 | t2 == t1 = (Arrow s t1)
 | otherwise = error ("Couldn't match expected: \n\t" ++ (show s) ++ "\n With actual type: \n\t" ++ (show t2) )
 where
  t2 = typeChecker (Ok e) env
  env = (x, s) : env

-- Function application
typeChecker (Ok(App e1 e2)) env
 | s1 == s2 = t
 | otherwise = error ((show s1) ++ " " ++ (show s2))
 where
  (Arrow s1 t) = typeChecker (Ok e1) env
  s2 = typeChecker(Ok e2) env




typeChecker (Ok(List e)) env
 | len types == 0 = EmptyList
 | hwAll (== IntType) types = IntList
 | hwAll (== BoolType) types = BoolList
 | hwAll (== FloatType) types = FloatList
 | otherwise = error "List of mixed types is not allowed"
 where
  types = map (\e -> typeChecker (Ok e) env) e


typeChecker (Ok(Cons e1 e2)) env
 | t1 == IntType && (t2 == IntList || t2 == EmptyList) = IntList
 | t1 == FloatType && (t2 == FloatList || t2 == EmptyList) = FloatList
 | t1 == BoolType && (t2 == BoolList || t2 == EmptyList) = BoolList
 | otherwise = error "List construction type mismatch."
 where 
  t1 = typeChecker (Ok (e1)) env
  t2 = typeChecker (Ok(e2)) env


typeChecker (Ok(Concat e1 e2)) env 
 | t1 == EmptyList && t2 == EmptyList = EmptyList
 | (t1 == BoolType || t1 == EmptyList) && (t2 == BoolType || t2 == EmptyList) = BoolList
 | (t1 == IntList || t1 == EmptyList) && (t2 == IntList || t2 == EmptyList) = IntList
 | t1 == FloatList && (t2 == IntList || t2 == FloatList || t2 == EmptyList) = FloatList
 | t1 == IntList && t2 == FloatList = FloatList
 | t1 == EmptyList && t2 == FloatList = FloatList
 | t1 == EmptyList && t2 == IntList = IntList
 
 | otherwise = error "List concatenation type mismatch."
 where 
  t1 = typeChecker (Ok (e1)) env
  t2 = typeChecker (Ok (e2)) env


typeChecker (Ok(Not)) env = Arrow BoolType BoolType




typeChecker e _ = error ("Unknown type: " ++ show(e))

-- http://www.zvon.org/other/haskell/Outputprelude/all_f.html
-- Rewrote `all`, just passes a condition to all children
hwAll :: (a -> Bool) -> [a] -> Bool
hwAll _ [] = True
hwAll cond (it:list) | cond it = hwAll cond list
hwAll cond (it:list) = False

len :: [a] -> Int
len [] = 0
len (_:list) = 1 + len list
