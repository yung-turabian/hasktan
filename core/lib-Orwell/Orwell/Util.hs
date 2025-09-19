{-# LANGUAGE DeriveDataTypeable #-}

module Orwell.Util (
    E(..),
    AST(..),
    TypeExp(..),
    OpEnv(..),
    TypeEnv(..),
    showType,
    formatErrorCode,
    ErrorCode(..)
) where

import Data.Data (Data, Typeable, toConstr, showConstr)

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

     | EOL
     deriving (Eq, Ord, Data)

showList :: [AST] -> String 
showList [] = ""
showList [l] = show l
showList (it:l) = show it ++ ", " ++ Orwell.Util.showList l

showType :: AST -> String
showType (Integer _) = "Int"
showType (Float _) = "Float"
showType (Char _) = "Char"
showType (Boolean _) = "Bool"
showType _ = "?"

instance Show AST where
     show (Integer n) = show n
     show (Boolean b) = show b
     show (Float f)   = show f
     show (List l) = "[" ++ Orwell.Util.showList l ++ "]"

     show (Plus l r) = show l ++ "(+)" ++ show r

     -- catch-all fallback
     show other       = "<AST:" ++ showConstr (toConstr other) ++ ">"


-- | Operation environment
type OpEnv = [(String,AST)]

-- | Type alias for type environments.
type TypeEnv = [(String,TypeExp)]

data ErrorCode
     = NotMemeberOfEnvironment
     | MismatchTypeInBind

instance Show ErrorCode where
     show NotMemeberOfEnvironment = "3"
     show MismatchTypeInBind = "4"

formatErrorCode :: ErrorCode -> String -> String
formatErrorCode errcode msg = "[HSQ-" ++ show errcode ++ "] " ++ msg
