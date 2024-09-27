{
-- Alex docs := https://haskell-alex.readthedocs.io/en/latest/syntax.html

-- Henry Wandover
-- CMSC 305
-- Due: October 18th, 2024
module Main (main) where
}

%wrapper "posn"
%encoding "utf-8"

$digit       = [0-9]
$alpha       = [a-zA-Z]
$punc        = [\{\}\[\]\;\:\"\'\,\.\`]
$symbol      = [\!\@\#\$\%\^\&\*\(\)\-\_\+\=\~\?\/\<\>\\]
$ascii       = [\n]
$all         = [$alpha $digit $punc $symbol $ascii]



@id   = $alpha [$alpha $digit \_ \']*

@string = $all # [\"] | " " -- Can't have a double-quote within a string that isnt end | Empty string


tokens :-

 $white+		        ; 
 "--".*                	  	;
 "{-"				;

 True | False                   { \p s -> BOOLVAL p (read s) }
 $digit+       	                { \p s -> INTVAL p (read s) }
 $digit+ \. $digit+		{ \p s -> FLOATVAL p (read s) }

 if				{ \p s -> IF p }
 then 				{ \p s -> THEN p }
 else 				{ \p s -> ELSE p }
 let	                        { \p s -> LET p }
 bind 				{ \p s -> BIND p }
 in            	                { \p s -> IN p }

 \\				{ \p s -> (LAMBDA) p }
 \-\>				{ \p s -> (ARROW) p }
 \:\:				{ \p s -> (COLONS) p }
 \=				{ \p s -> (EQUALS) p }
 \&\&				{ \p s -> (AND) p }
 \|\|				{ \p s -> (OR) p }
 \+				{ \p s -> (PLUS) p }
 \-				{ \p s -> (MINUS) p }
 \*				{ \p s -> (TIMES) p }
 \/				{ \p s -> (DIVIDE) p }
 \^				{ \p s -> (POWER) p }
 "quot"				{ \p s -> (QUOT) p }
 "rem"			       	{ \p s -> (REM) p }
 \<\= | "≤"			{ \p s -> (LTEQ) p }
 \>\= | "≥" 			{ \p s -> (GTEQ) p }
 \\\= | "≠"			{ \p s -> (NOTEQ) p }
 \<				{ \p s -> (LT') p }
 \>				{ \p s -> (GT') p}
 \(				{ \p s -> (LPAREN) p }
 \)				{ \p s -> (RPAREN) p }
 Bool				{ \p s -> (BOOL) p }
 Int				{ \p s -> (INT) p }
 Float				{ \p s -> (FLOAT) p }
 

 \" @string* \"			{ \p s -> STRING p (read s) }
 \[ 	     			{ \p s -> LBRACK p }
 \]				{ \p s -> RBRACK p }
 \,				{ \p s -> COMMA p }
 @id   				{ \p s -> VAR p s }

{

data Token
 = BOOLVAL  AlexPosn Bool
 | INTVAL   AlexPosn Int
 | FLOATVAL AlexPosn Float
 | IF       AlexPosn
 | THEN     AlexPosn
 | ELSE     AlexPosn
 | LET      AlexPosn
 | BIND     AlexPosn
 | IN       AlexPosn
 | LAMBDA   AlexPosn
 | ARROW    AlexPosn
 | COLONS   AlexPosn
 | EQUALS   AlexPosn
 | AND      AlexPosn
 | OR       AlexPosn
 | PLUS     AlexPosn
 | MINUS    AlexPosn
 | TIMES    AlexPosn
 | DIVIDE   AlexPosn
 | POWER    AlexPosn
 | QUOT     AlexPosn
 | REM      AlexPosn
 | LTEQ     AlexPosn
 | GTEQ     AlexPosn
 | NOTEQ    AlexPosn
 | LT'      AlexPosn
 | GT'      AlexPosn
 | BOOL     AlexPosn
 | INT      AlexPosn
 | FLOAT    AlexPosn
 | LPAREN   AlexPosn
 | RPAREN   AlexPosn
 | STRING   AlexPosn String
 | COMMA    AlexPosn
 | LBRACK   AlexPosn
 | RBRACK   AlexPosn
 | LIST     AlexPosn [Token]
 | SYMBOL   AlexPosn Char
 | VAR      AlexPosn String
 deriving (Eq, Show)

lexList :: String -> [Token]
lexList = alexScanTokens




main = do
 s <- getContents
 mapM_ print (alexScanTokens s)
}
