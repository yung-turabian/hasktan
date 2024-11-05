{
-- Henry Wandover
-- CMSC 305, Lab 4
-- Due: Friday, Nov. 8th 2024

module Grammar where
  
import Lexer
}

%name parseHasquelito
%tokentype { Token }
%error { parseError }
%monad { E } { thenE } { returnE }

%token

Int { INT p }
Bool { BOOL p }
Float { FLOAT p }

-- Constants
int { INTVAL p $$ }
float { FLOATVAL p $$ }
bool { BOOLVAL p $$ }

-- Keywords
let { LET p }
in  { IN p }
if { IF p}
then { THEN p }
else { ELSE p } 


'\\' { LAMBDA p }
"->" { ARROW p }
"::" { COLONS p }

--
"&&" { AND p }
"||" { OR p }

-- Mathematical operations
'=' { BIND p }
'+' { PLUS p }
'-' { MINUS p }
'*' { TIMES p }
'/' { DIVIDE p }

--
quot { QUOT p }
rem  { REM p }


-- Comparison
"==" { EQUALS p }
">=" { GTEQ p }
"<=" { LTEQ p }
"/=" { NOTEQ p }
'>'  { GT' p }
'<'  { LT' p }

-- Parenthesis
'(' { LPAREN p }
')' { RPAREN p }

-- Identifiers
var { VAR p $$ }

-- Lists
'[' { LBRACK p }
']' { RBRACK p }
':' { COLON p }
',' { COMMA p }
"++"{ PLUSPLUS p } 


{-
%nonassoc else in
%nonassoc "->"
%nonassoc "&&" "||"
%nonassoc '>' '<' "==" ">=" "<=" "/="

%left '+' '-'
%left '*' '/'
%nonassoc let
%right if then
%nonassoc int float bool
%nonassoc var '(' '\\' '='

   

%nonassoc APP
%nonassoc quot rem


%left NEG -}

%right in "->" else
%nonassoc '>' '<' "==" ">=" "<=" "/="
%nonassoc "&&" "||"
%right "++" ':'
%left '+' '-'
%left '*' '/'

%left Quot


%%

{-
AST :
  bool                                         { Boolean $1 }
| int                                          { Integer $1 }
| float                                        { Float $1 }

| let var '=' AST in AST                       { Let $2 $4 $6 }
| if AST then AST else AST                     { If $2 $4 $6 }
| '(' '\\'var "->" AST ')' "::" TypeExp        { Lambda $3 $5 $8 }

| AST AST                                      { App $1 $2 }

| AST "&&" AST                                 { And $1 $3 }
| AST "||" AST                                 { Or $1 $3 }

| AST '+' AST                                  { Add $1 $3 }
| AST '-' AST                                  { Sub $1 $3 }
| AST '*' AST                                  { Mul $1 $3 }
| AST '/' AST                                  { Div $1 $3 }

| quot AST AST                                 { Quot $2 $3 }
| rem AST AST                                  { Rem $2 $3 }

| '-' AST                                      { Sub (Integer 0) $2  }

| AST "==" AST                                 { Equals $1 $3 }
| AST ">=" AST                                 { Or (Equals $1 $3) (Gt $1 $3) }
| AST "<=" AST                                 { Or (Equals $1 $3) (Lt $1 $3) }
-- | AST "/=" AST                                 { NotEq $1 $3 } 
| AST '>' AST                                  { Gt $1 $3 }
| AST '<' AST                                  { Lt $1 $3 }

| '(' AST ')'                                  { $2 }

| var                                          { Variable $1 }-}


Expr : let var '=' Expr in Expr                         { Let $2 $4 $6 }
     | '(' '\\' var "->" Expr ')' "::" TypeExp "->" TypeExp         { Lambda $3 $5 $8 $10 }
     | if Expr then Expr else Expr                      { If $2 $4 $6 }
     
     | Expr "==" Expr              { Equals $1 $3 }
     | Expr ">=" Expr              { Or (Equals $1 $3) (Gt $1 $3) }
     | Expr "<=" Expr              { Or (Equals $1 $3) (Lt $1 $3) }
     | Expr "/=" Expr              { App (Not) (Equals $1 $3) } 
     | Expr '>' Expr               { Gt $1 $3 }
     | Expr '<' Expr               { Lt $1 $3 }

     | Expr "&&" Expr              { And $1 $3 }
     | Expr "||" Expr              { Or $1 $3 }

     | Expr ':' Expr               { Cons $1 $3 }
     | Expr "++" Expr              { Concat $1 $3 }
     | List                        { $1 }
     
     | Form                        { $1 }

Form : Form '+' Form               { Add $1 $3 }
     | Form '-' Form               { Sub $1 $3 }
     | Form '*' Form               { Mul $1 $3 }
     | Form '/' Form               { Div $1 $3 }
     
     | Juxt                        { $1 }

Juxt : Juxt Atom                   { App $1 $2 }
     | quot Atom Atom              { Quot $2 $3 }
     | rem Atom Atom               { Rem $2 $3 }
     | '-' Atom                    { Sub (Integer 0) $2 }

     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | int                         { Integer $1 }
     | bool                        { Boolean $1 }
     | float                       { Float $1 }
     | var                         { Variable $1 }
     


TypeExp : PrimType { $1 }
| TypeExp "->" TypeExp {Arrow $1 $3}
| '(' TypeExp ')' { $2 }


PrimType : Bool { BoolType } | Int { IntType } | Float { FloatType }


List : '[' ListMembers ']'          { List $2 }

ListMembers : {- empty -} { [] }
| Atom  { [$1] }
| Atom ',' ListMembers { $1 : $3 }


{

data E a
  = Ok a
  | Failed String
  deriving(Eq,Show)

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
  case m of
    Ok a      -> k a
    Failed e  -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
  case m of
    Ok a      -> Ok a
    Failed e  -> k e

parseError :: [Token] -> E a
parseError tokens = failE "Parse Error"


data TypeExp
  = BoolType
  | IntType
  | FloatType
  | Arrow TypeExp TypeExp
  deriving (Eq,Show)

data AST
  = Boolean Bool
  | Integer Int
  | Float Float
  
  | Let String AST AST
  | If AST AST AST

  | Lambda String AST TypeExp TypeExp

  | App AST AST

  | And AST AST
  | Or AST AST
  
  | Add AST AST
  | Sub AST AST
  | Mul AST AST
  | Div AST AST

  | Quot AST AST
  | Rem AST AST

  | Equals AST AST
  | Lt AST AST
  | Gt AST AST

  | Variable String

  | List [AST]
  | Cons AST AST
  | Concat AST AST

  | Not
  deriving (Eq,Show)

}