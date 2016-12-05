module Syntax where

import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map.Strict as Map
import Data.Maybe
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
    | TyPointer Type
    | TyBorrow Lifetime Qualifier Type
    deriving (Show, Eq)

type Location = Int

data Term
    = Var Name
    | Lit Integer
    | Seq [Term]
    | App Term Term
    | Pointer Lifetime Location
    | Alloc Lifetime Term
    | Borrow Lifetime Qualifier Term
    | Lam Lifetime Name Type Term Lifetime TermCtx
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

lookupIfVar :: HasVar a => a -> Map.Map Name a -> ThrowsError a
lookupIfVar val ctx = case getNameIfIsVar val of
    (Just s) -> getFromCtx s ctx
    Nothing -> return val

getFromCtx :: String -> Map.Map Name a -> ThrowsError a
getFromCtx name ctx = case Map.lookup name ctx of
    Just v -> return v
    Nothing -> throwError $ ErrVarNotFound name

type TypeCtx = Map.Map Name Type

type TermCtx = Map.Map Name Term

type LifetimeCtx = Map.Map Name Lifetime

type LocationCtx = Map.Map Name Location

type Heap = I.IntMap (Maybe Term)

type TypeHeap = I.IntMap (Maybe Type)

type ShadowHeap = Map.Map Name Shadow

type ThrowsError a = Except LangError a

data LangError
    = LangError String
    | ErrVarNotFound String
    deriving (Show)
