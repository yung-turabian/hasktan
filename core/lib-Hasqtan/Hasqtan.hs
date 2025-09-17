{-
    Public API for the Hasqtan lib.
-}
module Hasqtan(
    interp,
    typeCheckAndPrint,
    OpEnv(..),
    TypeEnv(..),
    Config(..),
    injectVariable,
    
    AST(..),
    TypeExp(..)
) where

import qualified Hasqtan.TypeChecker as TC
import Hasqtan.Interpreter
import Hasqtan.Util
import Hasqtan.Grammar
import Hasqtan.Lexer

import Control.Exception
import Data.Maybe

newtype Config = Config {
   shouldShowType :: Bool
}
    deriving (Eq)

-- | Makes it easy to print lists all nice and pretty.
formatResult :: E AST -> Bool -> Maybe String
formatResult (Ok EOL) _ = Nothing
formatResult (Ok lit) True = Just (show lit ++ " : " ++ showType lit)
formatResult (Ok lit) False = Just (show lit)
formatResult (Failed errorMsg) _ = Just errorMsg

typeCheckAndPrint :: String -> IO ()
typeCheckAndPrint s = do
   let ast = parseHasqtan (scanTokens s)
   let (tExp, env) = TC.typeCheck ast []
   print tExp

interp :: String -> (OpEnv, TypeEnv) -> Config -> (Maybe String, (OpEnv, TypeEnv))
interp s topEnv conf =
    let ast = parseHasqtan (scanTokens s)
        (oEnv, tEnv) = topEnv
        (tExp, tEnv') = TC.typeCheck ast tEnv
        (out, env) = case tExp of
            Failed msg -> (Just $ "error: " ++ msg, topEnv)
            Ok _ -> 
                let (val, oEnv') = runInterpreter ast oEnv
                    newEnv = (oEnv', tEnv')
                in (formatResult val (shouldShowType conf), newEnv)
    in
    (out, env)

injectVariable :: String -> AST -> TypeExp -> Maybe (OpEnv, TypeEnv) -> (OpEnv, TypeEnv)
injectVariable var_name var_ast var_typ maybeEnv =
    case maybeEnv of
        Just env ->
            let (opEnv, typeEnv) = env
                opEnv' = (var_name, var_ast):opEnv
                typeEnv' = (var_name, var_typ):typeEnv
            in
                (opEnv', typeEnv')
        Nothing ->
            let opEnv' = [(var_name, var_ast)]
                typeEnv' = [(var_name, var_typ)]
            in
                (opEnv', typeEnv')