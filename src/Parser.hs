module Parser where

import Control.Monad (void)
import Control.Monad.Except
import qualified Control.Monad.State.Lazy as S
import qualified Data.Map.Strict as Map

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Syntax

type CalcParser = S.StateT Int Parser

sc :: CalcParser ()
sc = L.space (void spaceChar) lineComment blockComment
    where
        lineComment = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

lexeme :: CalcParser a -> CalcParser a
lexeme = L.lexeme sc

symbol :: String -> CalcParser String
symbol = L.symbol sc

parens :: CalcParser a -> CalcParser a
parens = between (symbol"(") (symbol ")")

angles :: CalcParser a -> CalcParser a
angles =  between (symbol"<") (symbol ">")

integer :: CalcParser Integer
integer = lexeme L.integer

semi :: CalcParser String
semi = symbol ";"

reserved :: String -> CalcParser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

reservedOp :: String -> CalcParser ()
reservedOp s = void $ symbol s

reservedWords :: [String]
reservedWords = []

identifier :: CalcParser String
identifier = (lexeme . try) (p >>= check)
    where
        p = (:) <$> letterChar <*> many alphaNumChar
        check x
            | x `elem` reservedWords = fail $ "keyword " ++ show x ++ " cannot be S.identifier"
            | otherwise = return x

ltvar :: CalcParser Lifetime
ltvar = do
    name <- identifier
    return $ LifetimeVar name

calcParser :: CalcParser Term
calcParser = sc *> term <* eof

term :: CalcParser Term
term = parens term <|> termSeq

termSeq :: CalcParser Term
termSeq = f <$> sepBy1 term' semi
    where
        f l = if length l == 1 then head l else Seq l

term' :: CalcParser Term
term' = parens app
    <|> lam
    <|> intLit
    <|> borrow
    <|> alloc
    <|> var

app :: CalcParser Term
app = do
    func <- term
    param <- term
    return $ App func param

qualifier :: CalcParser Qualifier
qualifier = (reserved "mut" >> return Mut) <|> (reserved "imm" >> return Imm)

incLT :: CalcParser ()
incLT = S.modify (+ 1)

decLT :: CalcParser ()
decLT = S.modify ((-) 1)

lam :: CalcParser Term
lam = do
    void $ symbol "\\"
    lv <- angles ltvar
    name <- identifier
    void $ symbol ":"
    q <- qualifier
    ty <- typeExpr
    void $ symbol "."
    incLT
    body <- term
    decLT
    void $ symbol ":"
    funcTime <- ltvar
    return $ Lam lv name ty body funcTime Map.empty

borrow :: CalcParser Term
borrow = do
    reservedOp "&"
    lt <- S.get
    q <- qualifier
    e <- term
    return $ Borrow (LifetimeLit lt) q e

alloc :: CalcParser Term
alloc = do
    reserved "ref"
    lt <- S.get
    e <- term
    return $ Alloc (LifetimeLit lt) e

intLit :: CalcParser Term
intLit = integer >>= \n -> return $ Lit n

var :: CalcParser Term
var = identifier >>= \s -> return $ Var s

typeExpr = tyInt
    <|> tyBorrow
    <|> parens tyArrow
    <|> tyPointer
    <|> tyUnit

tyInt = (reserved "Int" >> return TyInt)

tyUnit = (reserved "Unit" >> return TyUnit)

tyBorrow = do
    reservedOp "&"
    q <- qualifier
    ty <- typeExpr
    return $ TyBorrow LTDummy q ty

tyArrow = do
    a <- typeExpr
    reservedOp "->"
    b <- typeExpr
    return $ TyArrow a b

tyPointer = do
    reservedOp "~"
    ty <- typeExpr
    return $ TyPointer ty

parseTerm :: String -> ThrowsError Term
parseTerm s = case runParser (S.runStateT term 0) "<input>" s of
    Right (term, _) -> return term
    Left err -> throwError $ LangError (show err)
