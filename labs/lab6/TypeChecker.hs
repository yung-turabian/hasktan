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

typeChecker:: AST -> TypeEnv -> TypeExp
typeChecker (Boolean _) _ = BoolType
typeChecker (Integer _) _ = IntType
typeChecker (Float _) _   = FloatType

typeChecker (Let _ _ _) _ = Let
typeChecker (If _ _ _) _ = If
typeChecker (Lambda _ _ _ _) _ = Lambda

typeChecker (App _ _) _ = App
typeChecker (And _ _) _ = And
typeChecker (Or _ _) _  = Or

typeChecker (Plus IntType IntType) _ = Plus
typeChecker (Minus _ _) _ = Minus
typeChecker (Times _ _) _ = Times
typeChecker (Divide _ _) _ = Divide

typeChecker (Quot _ _) _ = Quot
typeChecker (Rem _ _) _ = Rem
typeChecker (Equals _ _) _ = Equals
typeChecker (Gt _ _) _ = Gt
typeChecker (Lt _ _) _ = Lt

typeChecker (Variable s) env = TypeChecker.lookup s env

typeChecker (Cons _ _) = List
