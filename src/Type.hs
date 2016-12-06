module Type where

import qualified Data.IntMap.Strict as I

import Control.Monad.Except
import Control.Monad.State

import Syntax

data Typeinfo = Typeinfo
    { shadowHeap :: ShadowHeap
    , available :: [Location]
    , typeHeap :: TypeHeap
    }

-- type Typecheck a = ThrowsError (State Typeinfo) a

type Typecheck = StateT Typeinfo ThrowsError

allocateTy :: Type -> Typecheck Location
allocateTy ty = do
    tyInfo@(Typeinfo sh av th) <- get
    let (idx, rest) = splitHead av
    let newStore = I.insert idx ty th
    put tyInfo { typeHeap = newStore, available = rest }
    return idx

typecheck :: Term -> TypeCtx -> Typecheck (Type, TypeCtx)
typecheck term ctx = return (TyUnit, ctx)

derefTy :: Location -> Typecheck Type
derefTy loc = do
    st <- fmap typeHeap get
    case I.lookup loc st of
        Just ty -> return ty
        Nothing -> throwError $ ErrVarNotFound (show loc)

freeTy :: Location -> Typecheck ()
freeTy loc = do
    info@(Typeinfo sh av th) <- get
    case I.lookup loc th of
        Just ty -> do
            let newAvail = loc : av
            put $ info { available = newAvail }
        Nothing -> throwError $ ErrDefault "not a heap location"

-- canDeref :: Name -> Typecheck Bool
-- canDeref name = do
--     sh <- fmap shadowHeap get
--     case Map.lookup name sh of
--         Just v -> return $ canRead v
--         _ -> error $ "shadow heap does not contain " ++ name

emptyInfo = Typeinfo 
    { shadowHeap = emptyCtx
    , typeHeap = emptyHeap
    , available = [0..heapsize] 
    }

runTypecheck :: Term -> ThrowsError Type
runTypecheck term = fmap fst $ ran
    where ran = evalStateT (typecheck term emptyCtx) emptyInfo

