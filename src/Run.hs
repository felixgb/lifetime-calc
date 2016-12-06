module Run where

import Control.Monad.Except

import Syntax
import Parser
import Type
import Lifetime

process :: String -> ThrowsError (Type, Term)
process inp = do
    ast <- parseTerm inp
    ty <- runTypecheck ast
    lt <- runLt ast
    return (ty, ast)

runAll inp = case runExcept $ process inp of
    Right res -> show res
    Left err -> show err
