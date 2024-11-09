{
-- Alex docs := https://haskell-alex.readthedocs.io/en/latest/syntax.html

-- Henry Wandover
-- CMSC 305
-- Due: October 18th, 2024
-- Note: For extens, added strings, deliminited comments, lists and line/character numbers
module Lexer where
}

%wrapper "posn"
%encoding "utf-8"

$digit       = [0-9]
$alpha       = [a-zA-Z]
$punc        = [\{\}\[\]\;\:\"\'\,\.\`]
$symbol      = [\!\@\#\$\%\^\&\*\(\)\-\_\+\=\~\?\/\<\>\\]
$ascii       = [\n \32] -- \32 is ASCII code for a space
$all         = [$alpha $digit $punc $symbol $ascii]

@id          = $alpha [$alpha $digit \_ \']*

@string      = \" [$all # \"]* \" -- No double-quote within a string that isnt end | Empty string

tokens :-

 $white+		;

 -- Comments
 "--".*                	;
 "{-" ($all | \n)* "-}"	;


 -- Type definitions
 Bool			{ \p s -> (BOOL) p }
 Int			{ \p s -> (INT) p }
 Float			{ \p s -> (FLOAT) p }

 -- Constants
 True | False           { \p s -> BOOLVAL p (read s) }
 $digit+       	        { \p s -> INTVAL p (read s) }
 $digit+ \. $digit+	{ \p s -> FLOATVAL p (read s) }
 @string		{ \p s -> STRING p (read s) }

 -- Keywords
 if			{ \p s -> IF p }
 then 			{ \p s -> THEN p }
 else 		       	{ \p s -> ELSE p }
 let	    	       	{ \p s -> LET p }
 in            	        { \p s -> IN p }

 -- Arithmetic operators
 \\			{ \p s -> (LAMBDA) p }
 "+"			{ \p s -> (PLUS) p }
 "-"			{ \p s -> (MINUS) p }
 "*"			{ \p s -> (TIMES) p }
 "/"			{ \p s -> (DIVIDE) p }
 "="			{ \p s -> (BIND) p }
 "^"			{ \p s -> (POWER) p }
 quot			{ \p s -> (QUOT) p }
 rem			{ \p s -> (REM) p }

 -- Types
 "->"			{ \p s -> (ARROW) p }
 ":"			{ \p s -> (COLON) p }
 "::"			{ \p s -> (COLONS) p }

 -- Comparison
 "=="			{ \p s -> (EQUALS) p }
 "&&"			{ \p s -> (AND) p }
 "||"			{ \p s -> (OR) p }
 "<=" | "≤"		{ \p s -> (LTEQ) p }
 ">=" | "≥" 		{ \p s -> (GTEQ) p }
 "/=" | "≠"		{ \p s -> (NOTEQ) p }
 "<"			{ \p s -> (LT') p }
 ">"			{ \p s -> (GT') p }

 -- Parenthesis
 "("			{ \p s -> (LPAREN) p }
 ")"			{ \p s -> (RPAREN) p }
 
 -- Lists
 "["	     		{ \p s -> LBRACK p }
 "]"			{ \p s -> RBRACK p }
 ","			{ \p s -> COMMA p }
 "."			{ \p s -> PERIOD p }
 "++"                   { \p s -> PLUSPLUS p }


 @id   			{ \p s -> VAR p s }

{

data Token
-- Constants
 = BOOLVAL  AlexPosn Bool
 | INTVAL   AlexPosn Int
 | FLOATVAL AlexPosn Float
 | STRING   AlexPosn String
 
 -- Keywords
 | IF       AlexPosn
 | THEN     AlexPosn
 | ELSE     AlexPosn
 | LET      AlexPosn
 | IN       AlexPosn
 
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
 | QUOT     AlexPosn
 | REM      AlexPosn
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
 | INT      AlexPosn
 | FLOAT    AlexPosn
 
 -- Parenthesis
 | LPAREN   AlexPosn
 | RPAREN   AlexPosn
 
 -- Lists
 | COMMA    AlexPosn
 | PERIOD   AlexPosn
 | LBRACK   AlexPosn
 | RBRACK   AlexPosn
 | PLUSPLUS AlexPosn

 -- Identifiers
 | VAR      AlexPosn String



 | COMMENT  
 deriving (Eq, Show)


scanTokens = alexScanTokens
}
