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

-- typeChecker (Ok (Let _ _ _ ) ) _ = IntType
-- typeChecker (Ok (Let _ _ _ ) ) _ = BoolType
-- typeChecker (If _ _ _) _ = If
-- typeChecker (Lambda _ _ _ _) _ = Lambda

typeChecker (Ok (App _ _) ) _ = BoolType
typeChecker (Ok (And e1 e2 ) ) env | typeChecker (Ok (e1)) env == BoolType && typeChecker (Ok (e2)) env == BoolType = BoolType
typeChecker (Ok (Or e1 e2 ) ) env | typeChecker (Ok (e1)) env == BoolType && typeChecker (Ok (e2)) env == BoolType = BoolType


typeChecker (Ok (Plus e1 e2 ) ) env | typeChecker (Ok (e1)) env == IntType && typeChecker (Ok (e2)) env == IntType = IntType 
typeChecker (Ok (Plus e1 e2 ) ) env | typeChecker (Ok (e1)) env == FloatType && typeChecker (Ok (e2)) env == FloatType = FloatType 
typeChecker (Ok (Plus e1 e2 ) ) env | typeChecker (Ok (e1)) env == FloatType && typeChecker (Ok (e2)) env == IntType = FloatType 
typeChecker (Ok (Plus e1 e2 ) ) env | typeChecker (Ok (e1)) env == IntType && typeChecker (Ok (e2)) env == FloatType = FloatType 

typeChecker (Ok (Minus e1 e2 ) ) env | typeChecker (Ok (e1)) env == IntType && typeChecker (Ok (e2)) env == IntType = IntType 
typeChecker (Ok (Minus e1 e2 ) ) env | typeChecker (Ok (e1)) env == FloatType && typeChecker (Ok (e2)) env == FloatType = FloatType 
typeChecker (Ok (Minus e1 e2 ) ) env | typeChecker (Ok (e1)) env == FloatType && typeChecker (Ok (e2)) env == IntType = FloatType 
typeChecker (Ok (Minus e1 e2 ) ) env | typeChecker (Ok (e1)) env == IntType && typeChecker (Ok (e2)) env == FloatType = FloatType 

typeChecker (Ok (Times e1 e2 ) ) env | typeChecker (Ok (e1)) env == IntType && typeChecker (Ok (e2)) env == IntType = IntType 
typeChecker (Ok (Times e1 e2 ) ) env | typeChecker (Ok (e1)) env == FloatType && typeChecker (Ok (e2)) env == FloatType = FloatType 
typeChecker (Ok (Times e1 e2 ) ) env | typeChecker (Ok (e1)) env == FloatType && typeChecker (Ok (e2)) env == IntType = FloatType 
typeChecker (Ok (Times e1 e2 ) ) env | typeChecker (Ok (e1)) env == IntType && typeChecker (Ok (e2)) env == FloatType = FloatType 

typeChecker (Ok (Divide _ _ ) ) _ = FloatType 

typeChecker (Ok (Quot e1 e2 ) ) env | typeChecker (Ok (e1)) env == IntType && typeChecker (Ok (e2)) env == IntType = IntType 
typeChecker (Ok (Quot _ _ ) ) _ = error "Only supports use with Integers."

typeChecker (Ok (Rem e1 e2 ) ) env | typeChecker (Ok (e1)) env == IntType && typeChecker (Ok (e2)) env == IntType = IntType 
typeChecker (Ok (Rem _ _ ) ) _ = error "Only supports use with Integers."


typeChecker (Ok (Equals _ _) ) _ = BoolType
typeChecker (Ok (Gt _ _) ) _ = BoolType
typeChecker (Ok (Lt _ _) ) _ = BoolType


typeChecker (Ok (Variable s) ) env = TypeChecker.lookup s env

{-








typeChecker (Cons _ _) = List-}
