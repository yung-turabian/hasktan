{
-- Henry Wandover
-- October 18th, 2024

module Reva.Lexer
  (
    Alex,
    AlexPosn(..),
    alexGetInput,
    alexError,
    runAlex,
    alexMonadScan,
    Range(..),
    RangedToken(..),
    Token(..)
  ) where

import Reva.Util

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad (when)
}

%wrapper "monadUserState-bytestring"
%encoding "utf-8"

$digit       = [0-9]
$lower       = [a-z]
$upper       = [A-Z]
$alpha       = [$lower $upper]

$punc        = [\{\}\[\]\;\:\"\'\,\.\`]
$symbol      = [\!\@\#\$\%\^\&\*\(\)\-\_\+\=\~\?\/\<\>\\]
$ascii       = [\n \ ]
$all         = [$alpha $digit $punc $symbol]

$white_no_nl = [\ \t]

@id          = ($lower | \_) ($alpha | $digit | \_ | \' | \?)*

@tycon       = $upper [$alpha]*
@reservedid  = if|else|then|let|let|letrec|in|type
@reservedop  = ":" | "::" | "="

@string      = \"[$all # \"]*\" -- No double-quote within a string that isnt end | Empty string
@char        = \' [$all # \"\'] \'

@eol = \n | \r \n | \n \r
@empty_line = (($white_no_nl)*\n)

tokens :-

  <0> $white+              ;

  <0> "{-"                 { nestComment `andBegin` comment }
  <0> "-}"                 { \_ _ -> alexError "error: unexpected closing comment" }
  <comment> "{-"           { nestComment }
  <comment> "-}"           { unnestComment }
  <comment> .              ;
  <comment> \n             ;

  -- Keywords
  <0> let                  { tok T_Let }
  <0> letrec               { tok T_LetRec }
  <0> in                   { tok T_In }
  <0> if                   { tok T_If }
  <0> then                 { tok T_Then }
  <0> else                 { tok T_Else }
  <0> data                 { tok T_Data }
  -- Type defs
  <0> Int                  { tok T_Int }
  <0> Float                { tok T_Float }
  <0> Bool                 { tok T_Bool }
  -- Types
  <0> "->"                 { tok T_Arrow }
  <0> "::"                 { tok T_Colons }
  -- Arithmetic ops
  <0> "+"                  { tok T_Plus }
  <0> "-"                  { tok T_Minus }
  <0> "*"                  { tok T_Star }
  <0> "/"                  { tok T_Divide }
  <0> "^"                  { tok T_Power }
  -- Binding ops
  <0> \\                   { tok T_Lambda }
  <0> "="                  { tok T_Bind }
  -- Comparison ops
  <0> "=="                 { tok T_Eq }
  <0> "/="                 { tok T_NotEq }
  <0> ">"                  { tok T_Gt }
  <0> ">="                 { tok T_GtEq }
  <0> "<"                  { tok T_Lt }
  <0> "<="                 { tok T_LtEq }
  -- Logical ops
  <0> "&&"                 { tok T_And }
  <0> "||"                 { tok T_Or }
  -- Parenthesis
  <0> "("			             { tok T_LParen }
  <0> ")"			             { tok T_RParen }
  <0> "{"                   { tok T_LCurl }
  <0> "}"                   { tok T_RCurl }
  -- Lists
  <0> "["                   { tok T_LBrack }
  <0> "]"                   { tok T_RBrack }
  <0> ","                   { tok T_Comma }
  <0> "++"                  { tok T_PlusPlus }
  <0> ":"                   { tok T_Comma }


  <0> @id                  { tokId }

  -- Constants
  <0> $digit+              { tokInteger }
  <0> $digit* \. $digit+   { tokFloat }
  <0> True | False         { tokBool }
  <0> @string              { tokString }
  <0> @char                { tokChar }

{

data AlexUserState = AlexUserState {
  nestDepth :: Int
}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  {
    nestDepth = 0
  }

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

data Range = Range {
  start :: AlexPosn,
  end :: AlexPosn
} deriving (Eq,Show)

data RangedToken = RangedToken {
  rtToken :: Token,
  rtRange :: Range
} deriving (Eq,Show)

data Token
 = T_Ident     ByteString
 -- Constants
 | T_BoolVal   Bool
 | T_IntVal    Int
 | T_FloatVal     Float
 | T_StringVal    ByteString
 | T_CharVal   Char
 -- Keywords
 | T_If
 | T_Then
 | T_Else    
 | T_Let
 | T_LetRec
 | T_In
 | T_Data
 -- Logical operators
 | T_And 
 | T_Or    
 -- Arithmetic operators
 | T_Plus  
 | T_Minus   
 | T_Star
 | T_Divide  
 | T_Bind 
 | T_Power   
 | T_Lambda 
 -- Comparison operators
 | T_Eq
 | T_LtEq 
 | T_GtEq   
 | T_NotEq   
 | T_Lt  
 | T_Gt    
 -- Type definitions
 | T_Bool
 | T_Int    
 | T_Float  
 | T_Char
 | T_Arrow
 | T_Colon 
 | T_Colons 
 -- Parenthesis
 | T_LParen 
 | T_RParen
 | T_LCurl
 | T_RCurl  
 -- Lists
 | T_Comma
 | T_Period 
 | T_LBrack 
 | T_RBrack 
 | T_PlusPlus 
 | T_TyCon   ByteString
 | T_EOF
 deriving (Eq,Show)

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{ start=start, end = end }
  where
    end = BS.foldl' alexMove start $ BS.take len str

tokId :: AlexAction RangedToken
tokId inp@(_, _, str, _) len =
  pure RangedToken {
    rtToken = T_Ident $ BS.take len str,
    rtRange = mkRange inp len
  }

tokInteger :: AlexAction RangedToken
tokInteger inp@(_, _, str, _) len =
  pure RangedToken {
    rtToken = T_IntVal $ read $ BS.unpack $ BS.take len str,
    rtRange = mkRange inp len
  }

tokFloat:: AlexAction RangedToken
tokFloat inp@(_, _, str, _) len =
  pure RangedToken {
    rtToken = T_FloatVal $ read $ BS.unpack $ BS.take len str,
    rtRange = mkRange inp len
  }

tokString :: AlexAction RangedToken
tokString inp@(_, _, str, _) len =
  pure RangedToken {
    rtToken = T_StringVal $ BS.take len str,
    rtRange = mkRange inp len
  }

tokChar :: AlexAction RangedToken
tokChar inp@(_, _, str, _) len =
  pure RangedToken {
    rtToken = T_CharVal $ read $ BS.unpack $ BS.take len str,
    rtRange = mkRange inp len
  }

tokBool :: AlexAction RangedToken
tokBool inp@(_, _, str, _) len =
  pure RangedToken {
    rtToken = T_BoolVal $ read $ BS.unpack $ BS.take len str,
    rtRange = mkRange inp len
  }

tok :: Token -> AlexAction RangedToken
tok ctor inp len =
  pure RangedToken {
    rtToken = ctor,
    rtRange = mkRange inp len
  }

nestComment :: AlexAction RangedToken
nestComment input len = do
  modify $ \s -> s{ nestDepth = nestDepth s + 1 }
  skip input len

unnestComment :: AlexAction RangedToken
unnestComment input len = do
  state <- get
  let depth = nestDepth state - 1
  put state{ nestDepth = depth }
  when (depth == 0) $
    alexSetStartCode 0
  skip input len

alexEOF :: Alex RangedToken
alexEOF = do
  startCode <- alexGetStartCode
  when (startCode == comment) $
    alexError "error: unclosed comment"
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken T_EOF (Range pos pos)


}
