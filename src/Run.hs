module Run where

import Control.Exception
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

runThing :: String -> (Term -> ThrowsError a) -> a
runThing inp func = case runExcept $ parseTerm inp >>= func of
    Right success -> success
    Left err -> throw err

runAll inp = case runExcept $ process inp of
    Right res -> show res
    Left err -> show err

runFromFile :: String -> (Term -> ThrowsError a) -> IO a
runFromFile path func = readFile path >>= \s -> return $ runThing s func

