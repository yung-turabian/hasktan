{
-- Alex docs := https://haskell-alex.readthedocs.io/en/latest/syntax.html

-- Henry Wandover
-- CMSC 305
-- Due: October 18th, 2024
-- Note: For extens, added strings, deliminited comments, lists and line/character numbers

module Reva.Lexer where

import Reva.Util
}

%wrapper "posn"
%encoding "utf-8"

$digit       = [0-9]
$lower       = [a-z]
$upper       = [A-Z]
$alpha       = [$lower $upper]
$punc        = [\{\}\[\]\;\:\"\'\,\.\`]
$symbol      = [\!\@\#\$\%\^\&\*\(\)\-\_\+\=\~\?\/\<\>\\]
$ascii       = [\n \32] -- \32 is ASCII code for a space
$all         = [$alpha $digit $punc $symbol $ascii]
$white       = [\ \t\f\v\r]

@id          = $lower [$alpha $digit \_ \']*
@tycon       = $upper [$alpha]*
@reservedid  = if|else|then|let|let|letrec|in|type

@string      = \" [$all # \"]* \" -- No double-quote within a string that isnt end | Empty string
@char        = \' [$all # \"\'] \'

tokens :-
  
 $white+		               ;

 \n                        { \p _ -> T_Newline p }

 -- Comments
 "--"[^\n]*                ;
 "{-" ($all | \n)* "-}"	   ;


 -- Type definitions
 Bool			                 { \p _ -> (BOOL) p }
 Int			                 { \p _ -> T_Int p }
 Float	                   { \p _ -> (FLOAT) p }
 Char                      { \p _ -> CHAR p }

 -- Constants
 True | False              { \p s -> BOOLVAL p (read s) }
 $digit+       	           { \p s -> INTVAL p (read s) }
 $digit+ \. $digit+	       { \p s -> FLOATVAL p (read s) }
 @string		               { \p s -> STRING p (read s) }
 @char                     { \p s -> CHARVAL p (read s) }

 -- Keywords
 if			                   { \p s -> IF p }
 then 	                   { \p s -> THEN p }
 else 		                 { \p s -> ELSE p }
 let	                     { \p s -> LET p }
 letrec                    { \p s -> LET_REC p }
 in                        { \p s -> IN p }
 data                      { \p s -> DATA p }

 -- Arithmetic operators
 \\			                   { \p s -> LAMBDA p }
 "+"			                 { \p s -> PLUS p }
 "-"			                 { \p s -> MINUS p }
 "*"			                 { \p s -> TIMES p }
 "/"			                 { \p s -> DIVIDE p }
 "="			                 { \p s -> BIND p }
 "^"			                 { \p s -> POWER p }

 -- Types
 "->" | "→"                { \p s -> ARROW p }
 ":"			                 { \p s -> COLON p }
 "::"			                 { \p s -> COLONS p }

 -- Comparison
 "=="			           { \p s -> (EQUALS) p }
 "&&"			           { \p s -> (AND) p }
 "||"			           { \p s -> (OR) p }
 "<=" | "≤"		           { \p s -> (LTEQ) p }
 ">=" | "≥" 		       { \p s -> (GTEQ) p }
 "/=" | "≠"		           { \p s -> (NOTEQ) p }
 "<"			           { \p s -> (LT') p }
 ">"			           { \p s -> (GT') p }

 -- Parenthesis
 "("			           { \p s -> (LPAREN) p }
 ")"			           { \p s -> (RPAREN) p }
 "{"                       { \p s -> LCURL p }
 "}"                       { \p s -> RCURL p }

 -- Lists
 "["	     		       { \p s -> LBRACK p }
 "]"			           { \p s -> RBRACK p }
 ","			           { \p s -> COMMA p }
 "."			           { \p s -> PERIOD p }
 "++"                      { \p s -> PLUSPLUS p }

 hd                        { \p s -> HEAD p }
 tl                        { \p s -> TAIL p }

 @id   			           { \p s -> VAR p s }
 @tycon                    { \p s -> TYCON p s }

 .                         { \p s -> ERROR p s }

{

data Token
-- Constants
 = BOOLVAL  AlexPosn Bool
 | INTVAL   AlexPosn Int
 | FLOATVAL AlexPosn Float
 | STRING   AlexPosn String
 | CHARVAL  AlexPosn Char
 
 -- Keywords
 | IF       AlexPosn
 | THEN     AlexPosn
 | ELSE     AlexPosn
 | LET      AlexPosn
 | LET_REC  AlexPosn
 | IN       AlexPosn
 | DATA     AlexPosn
 
 -- Types
 | ARROW    AlexPosn
 | COLON   AlexPosn
 | COLONS  AlexPosn

 -- Logical operators
 | AND      AlexPosn
 | OR       AlexPosn

 -- Arithmetic operators
 | PLUS     AlexPosn
 | MINUS    AlexPosn
 | TIMES    AlexPosn
 | DIVIDE   AlexPosn
 | BIND     AlexPosn
 | POWER    AlexPosn
 | LAMBDA   AlexPosn

 -- Comparison operators
 | EQUALS   AlexPosn
 | LTEQ     AlexPosn
 | GTEQ     AlexPosn
 | NOTEQ    AlexPosn
 | LT'      AlexPosn
 | GT'      AlexPosn

 -- Type definitions
 | BOOL     AlexPosn
 | T_Int    AlexPosn
 | FLOAT    AlexPosn
 | CHAR     AlexPosn
 
 -- Parenthesis
 | LPAREN   AlexPosn
 | RPAREN   AlexPosn
 | LCURL    AlexPosn
 | RCURL    AlexPosn
 
 -- Lists
 | COMMA    AlexPosn
 | PERIOD   AlexPosn
 | LBRACK   AlexPosn
 | RBRACK   AlexPosn
 | PLUSPLUS AlexPosn

 | HEAD     AlexPosn
 | TAIL     AlexPosn

 | VAR      AlexPosn String
 | TYCON    AlexPosn String

 | ERROR    AlexPosn String

 | T_Newline AlexPosn
 | COMMENT  
 deriving (Eq)

instance Show Token where
    show (ERROR _ s) = s
    show tok = show tok

scanTokens = alexScanTokens

}
