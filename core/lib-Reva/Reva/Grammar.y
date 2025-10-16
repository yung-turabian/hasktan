{
-- Friday, Nov. 8th 2024

module Reva.Grammar where

import Reva.Lexer
import Reva.Util

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
}

%name parseReva program
%name parseInteractiveReva interactive
%error { parseError }
%monad { Alex } { >>= } { pure }
%lexer { lexer } { RangedToken T_EOF _ }
%expect 0

%tokentype { RangedToken }
%token


-- Identifiers
var { RangedToken (T_Ident _) _ }

-- Constants
integer   { RangedToken (T_IntVal _) _ }
float     { RangedToken (T_FloatVal _) _ }
bool      { RangedToken (T_BoolVal _) _ }
char      { RangedToken (T_CharVal _) _ }
string    { RangedToken (T_StringVal _) _ }

Int       { RangedToken (T_Int) _ }
Bool      { RangedToken (T_Bool) _ }
Float     { RangedToken (T_Float) _ }
Char      { RangedToken (T_Char) _ }

{-
-- Keywords
let { T_Let }
letrec { T_LetRec }
in  { T_In p }
if { T_If p}
then { T_Then p }
else { T_Else p }
data { T_Data p }

'\\' { T_Lambda p }
"->" { T_Arrow p }-}
"::" { RangedToken (T_Colons) _ }
{-
-- Logical ops
"&&" { T_And p }
"||" { T_Or p } -}

-- Mathematical operations
'=' { RangedToken (T_Bind) _ }
'+' { RangedToken (T_Plus) _ }
'-' { RangedToken (T_Minus) _ }
'*' { RangedToken (T_Star) _ }
'/' { RangedToken (T_Divide) _ }
'^' { RangedToken (T_Power) _ }

{-
-- Comparison
"==" { T_Eq p }
">=" { T_GtEq p }
"<=" { T_LtEq p }
"/=" { T_NotEq p }
'>'  { T_Gt p }
'<'  { T_Lt p }

-}

-- Parenthesis
'(' { RangedToken (T_LParen) _ }
')' { RangedToken (T_RParen) _ }


'{' { RangedToken (T_LCurl) _ }
'}' { RangedToken (T_RCurl) _ }

{-
-- Lists
'[' { T_LBrack p }
']' { T_RBrack p }
':' { T_Colon p }
',' { T_Comman p }
"++"{ T_PlusPlus p }

tycon { T_TyCon p $$ }

%left "->"
%right in else
%right "++" ':'
%nonassoc "&&" "||"
%nonassoc '>' '<' "==" ">=" "<=" "/=" 
%left '+' '-'
%left '*' '/'
%left '^'

-}

%%

-- Atoms are primitive types and are acted upon
-- Then Juxtaposed atoms have the next highest precidence, functions, applications and negation
-- Then following is form (arithematic operations) and expressions which have the lowest precidence.

{-

Interactive
  : Expr                     { $1 }
  | var "::" TypeExp '=' Expr
                             { Binding $1 $3 $5 }

{-DeclList
  : DeclList Newlines Decl   { $1 ++ [$3] }
  | {- empty -}              { [] }

Decl
  : DeclSingle '\n'               { $1 }
  | DeclFull '\n'                 { $1 }

DeclSingle
  : var "::" TypeExp '=' Expr 
                             { Binding $1 $3 $5 }
DeclFull
  : var "::" TypeExp Newlines var '=' Expr
                             { if $1 == $5
                               then Binding $1 $3 $7
                               else error "sig var doesnt match def var" }
-}
Vars
  : Vars var                 { $1 ++ [$2] }
  |                          { [] }

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
  | Expr "==" Expr           { Equals $1 $3 }
  | Expr ">=" Expr           { Or (Equals $1 $3) (Gt $1 $3) }
  | Expr "<=" Expr           { Or (Equals $1 $3) (Lt $1 $3) }
  | Expr "/=" Expr           { App (Not) (Equals $1 $3) } 
  | Expr '>' Expr            { Gt $1 $3 }
  | Expr '<' Expr            { Lt $1 $3 }
  | Expr "&&" Expr           { And $1 $3 }
  | Expr "||" Expr           { Or $1 $3 }
  | Expr ':' Expr            { Cons $1 $3 }
  | Expr "++" Expr           { Concat $1 $3 }


  --     | data tycon '=' tycon '{' RecordTypeFields '}' 
  --                         { RecordType $2 $4 $6 }

  | List                     { $1 }
  | Form                     { $1 }

Form :: { AST }
  : Form '+' Form            { Plus $1 $3 }
  | Form '-' Form            { Minus $1 $3 }
  | Form '*' Form            { Times $1 $3 }
  | Form '/' Form            { Divide $1 $3 }
	| Form '^' Form            { Power $1 $3 }
  | Juxt                     { $1 }

Juxt :: { AST }
--  : Juxt Atom                { App $1 $2 }
--  | '-' Atom                 { Minus (Integer 0) $2 }
  : Atom                     { $1 }

     
TypeExp 
  : PrimType                 { $1 }
  | TypeExp "->" TypeExp     {Arrow $1 $3}
  | '(' TypeExp ')'          { $2 }
  | '[' TypeExp ']'          { ListType $2 }
  | '[' {- empty -} ']'      { EmptyList }

PrimType 
  : Bool                     { BoolType } 
  | Int                      { IntType } 
  | Float                    { FloatType }
  | Char                     { CharType }

List 
  : '[' ListMembers ']'      { List $2 }

ListMembers 
  : {- empty -}              { [] }
  | Atom                     { [$1] }
  | ListMembers ',' Atom     { $1 ++ [$3] }

RecordTypeFields
  : {- empty -}              { [] }

-}

program :: { Ast Range }
  :  mainExpr       { $1 }

-- TODO maybe a main
mainExpr :: { Ast Range }
  : expr            { A_Expr $1 }
--  | {- empty -}     { unTok $1 (\range (T_)) }

interactive :: { Ast Range }
  : expr            { A_Expr $1 }

expr :: { Expr Range }
  : form            { $1 }

form
  : juxt            { $1 }

juxt
  : atom            { $1 }

atom :: { Expr Range }
  : integer         { unTok $1 (\range (T_IntVal int) -> E_Int range int) }
  | bool            { unTok $1 (\range (T_BoolVal bool) -> E_Bool range bool) }
  | float           { unTok $1 (\range (T_FloatVal float) -> E_Float range float) }
  | char            { unTok $1 (\range (T_CharVal char) -> E_Char range char) }
  | string          { unTok $1 (\range (T_StringVal string ) -> E_String range string) }
  | name            { E_Variable (info $1) $1 }
  | '(' ')'         { E_Unit (rtRange $1 <-> rtRange $2) }
  | '(' expr ')'    { E_Paren (rtRange $1 <-> rtRange $3) $2 }
  {-
--  | tycon                  { TypeConstructor $1 }-}

--type :: { TypeExp }
--  : name { T_Ident (info $1) }

name :: { Name Range }
  : var             { unTok $1 (\range (T_Ident name) -> Name range name) }

decl :: { Decl Range }
  : name '=' expr { Decl (info $1 <-> info $3) $1 [] Nothing $3 }
--  | name "::" type '=' expr { Decl (info $1 <-> info $3) $1 [] Nothing $3 }

{

-- | Build a simple node by extracting toke type and range.
unTok :: RangedToken -> (Range -> Token -> a) -> a
unTok (RangedToken tok range) ctor = ctor range tok

-- | Unsafely extract meta info from a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

-- | Union of two ranges, start of a1 -> end of b2
-- The LHS range must start before RHS range.
(<->) :: Range -> Range -> Range
Range a1 _ <-> Range _ b2 = Range a1 b2

parseError :: RangedToken -> Alex a
parseError _ = do
  (AlexPn _ line column, _, _, _) <- alexGetInput
  alexError $ "Parser error at line " <> show line <> ", col " <> show column

lexer :: (RangedToken -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

}
