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

typeChecker:: E AST -> TypeEnv -> TypeExp
typeChecker (Ok (Boolean _) ) _ = BoolType
typeChecker (Ok (Integer _) ) _ = IntType
typeChecker (Ok (Float _) ) _   = FloatType

typeChecker (Ok (Let s e1 e2) ) env 
 | typeChecker (Ok e1) env == expectedType = typeChecker (Ok e2) env
 where
  expectedType = typeChecker (Ok e1) env
  env = (s, expectedType) : env

   


typeChecker (Ok (If e1 e2 e3) ) env
 | typeChecker (Ok e1) env == BoolType && t1 == t2 = t1
 | typeChecker (Ok e1) env == BoolType && t1 == IntType && t2 == FloatType = t2
 | typeChecker (Ok e1) env == BoolType && t1 == FloatType && t2 == IntType = t1
 | otherwise = error "Check your IfThenElse formatting."
 where
  t1 = typeChecker (Ok e2) env
  t2 = typeChecker (Ok e3) env

-- TODO Correctly infer types from `e` or if App is used than accept the type of the argument for `s`
typeChecker (Ok (Lambda s e) ) env = typeChecker (Ok e) ( (s, (typeChecker (Ok e) env )) : env )


typeChecker (Ok (TypeSig e t1 t2) ) env = 
 let inferredType = typeChecker (Ok e) env 

 in if inferredType == t2 then t2 
 else error ("Couldn't match expected `" ++ (show t2) ++ "` with actual `" ++ (show t1) ++ "`.")


-- TODO Go over this again, typesig and lambda are confusing
typeChecker (Ok (App e1 e2) ) env = 
 let functionType = typeChecker (Ok e1) env
     argumentType = typeChecker (Ok e2) env
 in case e1 of

   Lambda s e -> 
    typeChecker (Ok e) ((s, argumentType) : env)

       
   TypeSig _ takesType returnsType ->
     if takesType == argumentType then takesType
     else error ("Function expected type `" ++ (show takesType) ++ "` but got `" ++ (show argumentType) ++ "` for argument.")


   _ -> -- No type signature
     if functionType == argumentType
     then functionType
     else error ("No instance for " ++ (show argumentType) ++ " -> " ++ (show functionType) ++ ".")


typeChecker (Ok (And e1 e2 ) ) env | typeChecker (Ok (e1)) env == BoolType && typeChecker (Ok (e2)) env == BoolType = BoolType
typeChecker (Ok (Or e1 e2 ) ) env | typeChecker (Ok (e1)) env == BoolType && typeChecker (Ok (e2)) env == BoolType = BoolType


typeChecker (Ok (Plus e1 e2 ) ) env 
 | t1 == IntType && t2 == IntType = IntType 
 | t1 == FloatType && (t2 == FloatType || t2 == IntType) = FloatType 
 | t1 == IntType && t2 == FloatType = FloatType
 where
  t1 = typeChecker (Ok e1) env
  t2 = typeChecker (Ok e2) env

typeChecker (Ok (Minus e1 e2 ) ) env 
 | t1 == IntType && t2 == IntType = IntType 
 | t1 == FloatType && (t2 == FloatType || t2 == IntType) = FloatType 
 | t1 == IntType && t2 == FloatType = FloatType
 where
  t1 = typeChecker (Ok e1) env
  t2 = typeChecker (Ok e2) env

typeChecker (Ok (Times e1 e2 ) ) env 
 | t1 == IntType && t2 == IntType = IntType 
 | t1 == FloatType && (t2 == FloatType || t2 == IntType) = FloatType 
 | t1 == IntType && t2 == FloatType = FloatType
 where
  t1 = typeChecker (Ok e1) env
  t2 = typeChecker (Ok e2) env

typeChecker (Ok (Divide e1 e2 ) ) env 
 | typeChecker (Ok (e1)) env == IntType || typeChecker (Ok (e1)) env == FloatType && 
   typeChecker (Ok (e2)) env == IntType || typeChecker (Ok (e1)) env == FloatType = FloatType 

typeChecker (Ok (Quot e1 e2 ) ) env 
 | typeChecker (Ok (e1)) env == IntType && typeChecker (Ok (e2)) env == IntType = IntType 
typeChecker (Ok (Quot _ _ ) ) _ = error "Only supports use with Integers."

typeChecker (Ok (Rem e1 e2 ) ) env 
 | typeChecker (Ok (e1)) env == IntType && typeChecker (Ok (e2)) env == IntType = IntType 
 | otherwise = error "Only supports use with Integers."


typeChecker (Ok (Equals e1 e2) ) env
 | t1 == t2 = BoolType
 | t1 == FloatType && t2 == IntType = BoolType 
 | t1 == IntType && t2 == FloatType = BoolType 
 | otherwise = error ("Mismatch types at `" ++ (show t1) ++ " == " ++ (show t2) ++ "`.")
 where
  t1 = typeChecker (Ok (e1)) env
  t2 = typeChecker (Ok (e2)) env

typeChecker (Ok (Gt e1 e2) ) env
 | typeChecker (Ok e1) env == typeChecker (Ok e2) env = BoolType
 | typeChecker (Ok (e1)) env == FloatType && typeChecker (Ok (e2)) env == IntType = BoolType 
 | typeChecker (Ok (e1)) env == IntType && typeChecker (Ok (e2)) env == FloatType = BoolType 
 | otherwise = error "Mismatch types >." 

typeChecker (Ok (Lt e1 e2) ) env
 | typeChecker (Ok e1) env == typeChecker (Ok e2) env = BoolType
 | typeChecker (Ok (e1)) env == FloatType && typeChecker (Ok (e2)) env == IntType = BoolType 
 | typeChecker (Ok (e1)) env == IntType && typeChecker (Ok (e2)) env == FloatType = BoolType 
 | otherwise = error "Mismatch types <." 



typeChecker (Ok (Variable s) ) env = TypeChecker.lookup s env

typeChecker (Ok (List e) ) env
 | len types == 0 = EmptyList
 | hwAll (== IntType) types = IntList
 | hwAll (== BoolType) types = BoolList
 | hwAll (== FloatType) types = FloatList
 | otherwise = error "List of mixed types is not allowed"
 where
  types = map (\e -> typeChecker (Ok e) env) e


typeChecker (Ok (Cons e1 e2) ) env
 | t1 == IntType && (t2 == IntList || t2 == EmptyList) = IntList
 | t1 == FloatType && (t2 == IntList || t2 == FloatList || t2 == EmptyList) = FloatList
 | t1 == IntType && t2 == FloatList = FloatList
 | t1 == BoolType && (t2 == BoolList || t2 == EmptyList) = BoolList
 | otherwise = error "List construction type mismatch."
 where 
  t1 = typeChecker (Ok (e1)) env
  t2 = typeChecker (Ok (e2)) env


typeChecker (Ok (Concat e1 e2) ) env 
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


typeChecker (Ok (Not) ) env = BoolType




typeChecker _ _ = error "Failed to type-check"
-- http://www.zvon.org/other/haskell/Outputprelude/all_f.html
-- Rewrote `all`, just passes a condition to all children
hwAll :: (a -> Bool) -> [a] -> Bool
hwAll _ [] = True
hwAll cond (it:list) | cond it = hwAll cond list
hwAll cond (it:list) = False

len :: [a] -> Int
len [] = 0
len (_:list) = 1 + len list
