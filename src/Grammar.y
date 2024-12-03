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
letrec { LET_REC p }
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
'^' { POWER p }

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
head { HEAD p }
tail { TAIL p }



%right in "->" else
%right "++" ':' head tail
%nonassoc "&&" "||"
%nonassoc '>' '<' "==" ">=" "<=" "/="
%nonassoc ')' "::"
%left '+' '-'
%left '*' '/'
%left '^'
%%

-- I preferred this way because it shows precidence in its structure.
-- Atoms are primitive types and are acted upon
-- Then Juxtaposed atoms have the next highest precidence, functions, applications and negation
-- Then following is form (arithematic operations) and expressions which have the lowest precidence.

Expr : let var '=' Expr in Expr                         { Let $2 $4 $6 }
     | letrec var '=' Expr in Expr { LetRec $2 $4 $6 }
     | '(' '\\' var "->" Expr ')' "::" TypeExp "->" TypeExp   { Lambda $3 $5 $8 $10 }
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
     | head Expr                   { Head $2 }
     | tail Expr                   { Tail $2 }
     | List                        { $1 }
     
     | Form                        { $1 }

Form : Form '+' Form               { Plus $1 $3 }
     | Form '-' Form               { Minus $1 $3 }
     | Form '*' Form               { Times $1 $3 }
     | Form '/' Form               { Divide $1 $3 }
		 | Form '^' Form               { Power $1 $3 }
     
     | Juxt                        { $1 }

Juxt : Juxt Atom                   { App $1 $2 }
     | quot Atom Atom              { Quot $2 $3 }
     | rem Atom Atom               { Rem $2 $3 }
     | '-' Atom                    { Minus (Integer 0) $2 }

     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | int                         { Integer $1 }
     | bool                        { Boolean $1 }
     | float                       { Float $1 }
     | var                         { Variable $1 }
     


TypeExp : PrimType { $1 }
| TypeExp "->" TypeExp {Arrow $1 $3}
| '(' TypeExp ')' { $2 }
| '[' TypeExp ']' { ListType $2 }
| '[' {- empty -} ']' { EmptyList }


PrimType : Bool { BoolType } | Int { IntType } | Float { FloatType }


List : '[' ListMembers ']'          { List $2 }

ListMembers : {- empty -} { [] }
| Atom  { [$1] }
| Atom ',' ListMembers { $1 : $3 }


{

data E a = Ok a | Failed String
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

parseError :: [Token] -> E a
parseError [] = failE "Parse Error: Unexpected end of input"
parseError (t:_) = failE $ "Parse Error: Unexpected token " ++ show t


data TypeExp
  = BoolType
  | IntType
  | FloatType
   
  | Arrow TypeExp TypeExp
  
  | EmptyList
  | ListType TypeExp
  deriving (Eq,Ord,Show)

data AST
  = Boolean Bool
  | Integer Int
  | Float Float
  
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

  | Quot AST AST
  | Rem AST AST

  | Equals AST AST
  | Lt AST AST
  | Gt AST AST

  | Variable String

  | List [AST]
  | Cons AST AST
  | Concat AST AST
  | Head AST
  | Tail AST

  | Not
  deriving (Eq,Ord,Show)

}
