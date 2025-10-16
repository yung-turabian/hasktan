{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}

module Reva.Util (
    E(..),
    AST(..),
    Ast(..),
    Expr(..),
    Decl(..),
    Name(..),
    TypeExp(..),
    OpEnv(..),
    TypeEnv(..),
    showType,
    formatErrorCode,
    ErrorCode(..)
) where

import Data.Data (Data, Typeable, toConstr, showConstr)
import Data.ByteString.Lazy.Char8 (ByteString)

-- | Tracks successful AST or error codes if not.
data E a = Ok a | Failed String
    deriving(Eq)

instance Show a => Show (E a) where
     show (Ok a) = show a
     show (Failed str) = str

data TypeExp
  = BoolType
  | IntType
  | FloatType
  | CharType
  | VoidType
   
  | Arrow TypeExp TypeExp
  | EmptyList
  | ListType TypeExp
  deriving (Eq,Ord,Data)

instance Show TypeExp where
  show IntType       = "Int"
  show BoolType      = "Bool"
  show FloatType     = "Float"
  show CharType      = "Char"
  show VoidType      = "()"
  show (Arrow t1 t2) = show t1 ++ " -> " ++ show t2
  show EmptyList     = "[]"
  show (ListType t)  = "[" ++ show t ++ "]"

data Name a
  = Name a ByteString
  deriving (Foldable,Show)

-- Use TypeExp with the polymorphic a

data Argument a
  = Argument (Name a) (Maybe TypeExp)
  deriving (Foldable,Show)

data Decl a
  = Decl a (Name a) [Argument a] (Maybe TypeExp) (Expr a)
  deriving (Foldable,Show)

data Expr a
  = E_App a (Expr a) (Expr a)
  | E_Int a Int
  | E_Float a Float
  | E_Char a Char
  | E_Bool a Bool
  | E_Variable a (Name a)
  | E_String a ByteString
  | E_Paren a (Expr a)
  | E_Unit a -- VoidType
  deriving (Foldable)

data Ast a
  = A_Expr (Expr a)
  | A_Decl (Decl a)
  deriving (Foldable,Show)

instance Show (Expr a) where
  show (E_Int _ i)   = show i
  show (E_Float _ f) = show f
  show (E_Char _ c)  = show c
  show (E_Bool _ b)  = show b
  show e             = show e

data AST
     = Program [AST] (Maybe AST) -- Decl. list and main expr

     | Boolean Bool
     | Integer Int
     | Float Float
     | Char Char
     
     | Let String AST AST
     | LetRec String AST AST
     | If AST AST AST

     | Lambda String AST TypeExp TypeExp

     | App AST AST

     | And AST AST
     | Or AST AST
     
     | Plus AST AST
     | Minus AST AST
     | Times AST AST
     | Divide AST AST
     | Power AST AST

     | Equals AST AST
     | Lt AST AST
     | Gt AST AST

     | Variable String
     -- | RecordType String String [AST]

     | List [AST]
     | Cons AST AST
     | Concat AST AST

     | Head AST
     | Tail AST

     | Binding String TypeExp AST

     | Not
     deriving (Eq, Ord, Data)

showList :: [AST] -> String 
showList [] = ""
showList [l] = show l
showList (it:l) = show it ++ ", " ++ Reva.Util.showList l

showType :: Expr a -> String
showType (E_Int _ _) = "Int"
showType (E_Float _ _) = "Float"
showType (E_Char _ _) = "Char"
showType (E_Bool _ _) = "Bool"
showType _ = "?"

instance Show AST where
     show (Integer n)  = show n
     show (Boolean b)  = show b
     show (Float f)    = show f
     show (Variable v) = v
     show (List l) = "[" ++ Reva.Util.showList l ++ "]"

     -- catch-all fallback
     show other       = "<AST:" ++ showConstr (toConstr other) ++ ">"


-- | Operation environment
type OpEnv = [(String,AST)]

-- | Type alias for type environments.
type TypeEnv = [(String,TypeExp)]

data ErrorCode
     = NotMemeberOfEnvironment
     | MismatchTypeInBind
     | MismatchTypeInBinaryOp

instance Show ErrorCode where
     show NotMemeberOfEnvironment = "3"
     show MismatchTypeInBind = "4"
     show MismatchTypeInBinaryOp = "5"

formatErrorCode :: ErrorCode -> String -> String
formatErrorCode errcode msg = "[HSQ-" ++ show errcode ++ "] " ++ msg
