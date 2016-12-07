module Syntax where

import Text.Megaparsec.Error

import Control.Exception
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Typeable
import qualified Data.IntMap.Strict as I

type Name = String

type TermEnv = Map.Map Name Term

class HasVar a where
    getNameIfIsVar :: a -> Maybe String

data Lifetime 
    = LifetimeLit Int
    | LifetimeVar Name
    | LTDummy
    deriving (Eq, Show)

instance HasVar Lifetime where
    getNameIfIsVar (LifetimeVar n) = Just n
    getNameIfIsVar other = Nothing

data Qualifier
    = Imm
    | Mut
    deriving (Eq, Show)

data Type
    = TyArrow Type Type
    | TyInt
    | TyUnit
    | TyPointer Lifetime Type
    | TyBorrow Lifetime Qualifier Type
    deriving (Show, Eq)

type Location = Int

data Term
    = Var Name
    | Let Name Term Term
    | Lit Integer
    | Seq [Term]
    | App Term Term
    | Pointer Lifetime Location
    | Alloc Lifetime Term
    | Borrow Lifetime Qualifier Term
    | Lam Lifetime Lifetime Name Type Term Lifetime TermCtx
    deriving (Show, Eq)

instance HasVar Term where
    getNameIfIsVar (Var n) = Just n
    getNameIfIsVar other = Nothing

data Shadow
    = SUninit
    | SInit
    | SPointer Shadow
    | SBorrow Qualifier Shadow
    deriving (Show, Eq)

-- CONTEXT STUFF

extend :: HasVar a => a -> a -> Map.Map Name a -> ThrowsError (Map.Map Name a)
extend key val ctx = case getNameIfIsVar key of
    (Just s) -> return $ Map.insert s val ctx
    Nothing -> throwError $ ErrNotAVar

addToContext :: Name -> a -> Map.Map Name a -> Map.Map Name a
addToContext = Map.insert

lookupIfVar :: HasVar a => a -> Map.Map Name a -> ThrowsError a
lookupIfVar val ctx = case getNameIfIsVar val of
    (Just s) -> getFromCtx s ctx
    Nothing -> return val

getFromCtx :: String -> Map.Map Name a -> ThrowsError a
getFromCtx name ctx = case Map.lookup name ctx of
    Just v -> return v
    Nothing -> throwError $ ErrVarNotFound name

-- HEAP STUFF

heapsize :: Int
heapsize = 100

splitHead (x:xs) = (x, xs)

-- DEFINITIONS

emptyCtx = Map.empty

emptyHeap = I.empty

type TypeCtx = Map.Map Name Type

type TermCtx = Map.Map Name Term

type LifetimeCtx = Map.Map Name Lifetime

type LocationCtx = Map.Map Name Location

type Heap = I.IntMap (Maybe Term)

type TypeHeap = I.IntMap Type

type ShadowHeap = Map.Map Name Shadow

-- Error reporting

type ThrowsError = Except LangError

data LangError
    = ErrParse String
    | ErrDefault String
    | ErrVarNotFound String
    | ErrLifetimeViolation
    | ErrNotAVar
    deriving (Show, Eq, Typeable)

instance Exception LangError
