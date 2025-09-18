{
-- Henry Wandover
-- CMSC 305, Lab 4
-- Due: Friday, Nov. 8th 2024

module Orwell.Grammar where
  
import Orwell.Lexer
import Orwell.Util

}

%name parseOrwell Program
%tokentype { Token }
%error { parseError }
%monad { E } { thenE } { returnE }

%token

Int   { INT p }
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

'\n' { NEWLINE p }

%right in "->" else
%right "++" ':' hd tl
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

-- Top-level declarations, not available in interactive mode.
Program
     : var "::" TypeExp '\n' var '=' Expr
                                   { Binding $1 $3 $5 $7 }
     | var "::" TypeExp '=' Expr   { Binding $1 $3 $1 $5 } -- Interactive
     | Expr                        { $1 }

Expr 
     : let var '=' Expr in Expr    
                                   { Let $2 $4 $6 }
     | letrec var '=' Expr in Expr 
                                   { LetRec $2 $4 $6 }
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

Form 
     : Form '+' Form               { Plus $1 $3 }
     | Form '-' Form               { Minus $1 $3 }
     | Form '*' Form               { Times $1 $3 }
     | Form '/' Form               { Divide $1 $3 }
	| Form '^' Form               { Power $1 $3 }
     | Juxt                        { $1 }

Juxt 
     : Juxt Atom                   { App $1 $2 }
     | quot Atom Atom              { Quot $2 $3 }
     | rem Atom Atom               { Rem $2 $3 }
     | '-' Atom                    { Minus (Integer 0) $2 }
     | Atom                        { $1 }

Atom 
     : '(' Expr ')'                { $2 }
     | int                         { Integer $1 }
     | bool                        { Boolean $1 }
     | float                       { Float $1 }
     | char                        { Char $1 }
     | var                         { Variable $1 }
--     | tycon                       { TypeConstructor $1 }
     
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
     | Atom ',' ListMembers { $1 : $3 }

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
