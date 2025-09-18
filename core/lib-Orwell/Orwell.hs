{-
    Public API for the Orwell lib.
-}
module Orwell(
    interp,
    typeCheckAndPrint,
    OpEnv(..),
    TypeEnv(..),
    Config(..),
    injectVariable,
    
    AST(..),
    TypeExp(..)
) where

import qualified Orwell.TypeChecker as TC
import Orwell.Interpreter
import Orwell.Util
import Orwell.Grammar
import Orwell.Lexer

import Control.Exception
import Data.Maybe

foreign import ccall "max" c_max :: Int -> Int -> Int

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
   let ast = parseOrwell (scanTokens s)
   let (tExp, env) = TC.typeCheck ast []
   print tExp

interp :: String -> (OpEnv, TypeEnv) -> Config -> (Maybe String, (OpEnv, TypeEnv))
interp s topEnv conf =
    let ast = parseOrwell (scanTokens s)
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
<<<<<<< HEAD:core/lib-Hasqtan/Hasqtan.hs
                return (opEnv', typeEnv')
=======
                return (opEnv', typeEnv')
>>>>>>> 7a73feb8b2adfb5c681340b25842d0b4c7560832:core/lib-Orwell/Orwell.hs
