{
-- Henry Wandover
-- CMSC 305, Lab 4
-- Due: Friday, Nov. 8th 2024

module Orwell.Grammar where
import Orwell.Lexer
import Orwell.Util

}

%name parseOrwell Program
%name parseInteractiveOrwell Interactive
%error { parseError }
%monad { E } { thenE } { returnE }

%tokentype { Token }
%token

Int   { T_Int p }
Bool  { BOOL p }
Float { FLOAT p }
Char  { CHAR p }

-- Constants
int   { INTVAL p $$ }
float { FLOATVAL p $$ }
bool  { BOOLVAL p $$ }
char  { CHARVAL p $$ }

-- Keywords
let { LET p }
letrec { LET_REC p }
in  { IN p }
if { IF p}
then { THEN p }
else { ELSE p }
data { DATA p }


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
'^' { POWER p }

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

'{' { LCURL p }
'}' { RCURL p }

-- Identifiers
var { VAR p $$ }

-- Lists
'[' { LBRACK p }
']' { RBRACK p }
':' { COLON p }
',' { COMMA p }
"++"{ PLUSPLUS p }

hd { HEAD p }
tl { TAIL p }

tycon { TYCON p $$ }

'\n' { T_Newline p }

%right in "->" else
%right "++" ':' hd tl
%nonassoc "&&" "||"
%nonassoc '>' '<' "==" ">=" "<=" "/="
%nonassoc ')' "::"
%left '+' '-'
%left '*' '/'
%left '^'

%%

-- Atoms are primitive types and are acted upon
-- Then Juxtaposed atoms have the next highest precidence, functions, applications and negation
-- Then following is form (arithematic operations) and expressions which have the lowest precidence.

Program :: { AST }
  : DeclList MainExpr           { Program $1 $2 }

MainExpr :: { Maybe AST }
  : Newlines Expr Newlines      { Just $2 }
  |                             { Nothing }

Newlines
  : '\n' Newlines               { () }
  |                             { () }

Interactive
  : Expr                        { $1 }
  | var "::" TypeExp '=' Expr   { Binding $1 $3 $5 }

DeclList
  : DeclList Decl               { $1 ++ [$2] }
  | {- empty -}                 { [] }

Decl
  : DeclSingle '\n'             { $1 }
  | DeclFull '\n'               { $1 }

DeclSingle
  : var "::" TypeExp '=' Expr 
                                { Binding $1 $3 $5 }
DeclFull
  : var "::" TypeExp '\n' var '=' Expr
                                { if $1 == $5
                                  then Binding $1 $3 $7
                                  else error "sig var doesnt match def var" }

Expr :: { AST }
  : let var '=' Expr in Expr    
                                { Let $2 $4 $6 }
  | letrec var '=' Expr in Expr 
                                { LetRec $2 $4 $6 }
  -- Requires input so we write an explicit arrow type.
  | '(' '\\' var "->" Expr ')' "::" TypeExp "->" TypeExp   
                                { Lambda $3 $5 $8 $10 }
  | if Expr then Expr else Expr                      
                                { If $2 $4 $6 }
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


  --     | data tycon '=' tycon '{' RecordTypeFields '}' 
  --                                   { RecordType $2 $4 $6 }

   -- Function calls, TODO abstract these kinds
  | hd Expr                     { Head $2 }
  | tl Expr                     { Tail $2 }

  | List                        { $1 }
  | Form                        { $1 }

Form :: { AST }
     : Form '+' Form            { Plus $1 $3 }
     | Form '-' Form            { Minus $1 $3 }
     | Form '*' Form            { Times $1 $3 }
     | Form '/' Form            { Divide $1 $3 }
	   | Form '^' Form            { Power $1 $3 }
     | Juxt %shift              { $1 }

Juxt :: { AST }
     : Juxt Atom                { App $1 $2 }
--     | '-' Atom                 { Minus (Integer 0) $2 }
     | Atom                     { $1 }

Atom :: { AST }
     : int                      { Integer $1 }
     | bool                     { Boolean $1 }
     | float                    { Float $1 }
     | char                     { Char $1 }
     | var                      { Variable $1 }
     | '(' Expr ')'             { $2 }
--     | tycon                  { TypeConstructor $1 }
     
TypeExp 
     : PrimType { $1 }
     | TypeExp "->" TypeExp {Arrow $1 $3}
     | '(' TypeExp ')' { $2 }
     | '[' TypeExp ']' { ListType $2 }
     | '[' {- empty -} ']' { EmptyList }

PrimType 
     : Bool { BoolType } 
     | Int { IntType } 
     | Float { FloatType }

List 
     : '[' ListMembers ']'    { List $2 }

ListMembers 
     : {- empty -}          { [] }
     | Atom                 { [$1] }
     | ListMembers ',' Atom { $1 ++ [$3] }

RecordTypeFields
     : {- empty -}            { [] }


{

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
  case m of
    Ok a      -> k a
    Failed e  -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

parseError :: [Token] -> E a
parseError [] = failE "1] Parse Error: Unexpected end of input"
-- TODO: Would ideally use Alex's monad wrapper, this works for now.
parseError (t:_) = failE $ "2] Parse Error: Unexpected token: '" ++ show t ++ "'."

}
