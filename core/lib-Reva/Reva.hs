{-
    Public API for the Reva lib.
-}
module Reva(
    interp,
    typeCheckAndPrint,
    OpEnv(..),
    TypeEnv(..),
    Config(..),
    injectVariable,
    
    AST(..),
    TypeExp(..),
    Mode(..)
) where

import qualified Reva.TypeChecker as TC
import Reva.Interpreter
import Reva.Util
import Reva.Grammar
import Reva.Lexer

import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Control.Exception
import Data.Maybe

--foreign import ccall "max" c_max :: Int -> Int -> Int

newtype Config = Config {
   shouldShowType :: Bool
}
    deriving (Eq)

data Mode 
  = CMD
  | REPL

-- | Makes it easy to print lists all nice and pretty.
formatResult :: Either String (Expr ()) -> Bool -> Maybe String
formatResult (Right lit) True = Just (show lit ++ " : " ++ showType lit)
formatResult (Right lit) False = Just (show lit)
formatResult (Left errorMsg) _ = Just errorMsg

typeCheckAndPrint :: ByteString -> IO ()
typeCheckAndPrint s = do
  let ast = runAlex s parseReva
  --let (tExp, env) = TC.typeCheck ast []
  --print tExp
  print $ show ast

interp :: Mode -> String -> (OpEnv, TypeEnv) -> Config -> (Maybe String, (OpEnv, TypeEnv))
interp mode s topEnv conf =
    let inp = pack s
        ast = case mode of
          CMD -> runAlex inp parseReva
          -- REPL -> runAlex s parseInteractiveReva
        (oEnv, tEnv) = topEnv
        (tExp, tEnv') = TC.typeCheck ast tEnv
        (out, env) = case tExp of
            Left msg -> (Just $ "error: " ++ msg, topEnv)
            Right _ -> 
                let (val, oEnv') = runInterpreter ast oEnv
                    newEnv = (oEnv', tEnv')
                in (formatResult val (shouldShowType conf), newEnv)
    in
    (out, env)

injectVariable :: String -> AST -> TypeExp -> Maybe (OpEnv, TypeEnv) -> IO (OpEnv, TypeEnv)
injectVariable var_name var_ast var_typ maybeEnv =
    case maybeEnv of
        Just env ->
            let (opEnv, typeEnv) = env
                opEnv' = (var_name, var_ast):opEnv
                typeEnv' = (var_name, var_typ):typeEnv
            in
                return (opEnv', typeEnv')
        Nothing ->
            let opEnv' = [(var_name, var_ast)]
                typeEnv' = [(var_name, var_typ)]
            in
                return (opEnv', typeEnv')
