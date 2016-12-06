module Lifetime where

import Control.Monad (unless)
import Control.Monad.Except

import Syntax

lifetimeLT :: Lifetime -> Lifetime -> LifetimeCtx -> ThrowsError Bool
lifetimeLT a b ctx = do
    a' <- lookupIfVar a ctx
    b' <- lookupIfVar b ctx
    case a' of
        LifetimeLit n1 -> case b' of
            LifetimeLit n2 -> return (n1 <= n2)
            LTDummy -> return True

-- populate dummy borrows
ltWalk :: Term -> Term
ltWalk term = case term of
    (App e1 e2) -> App (ltWalk e1) (ltWalk e2)

    (Seq ts) -> Seq (map ltWalk ts)

    (Lam flt lt name ty body retLt clos) -> Lam flt lt name ty (ltWalk body) retLt clos

    (Borrow _ qual p@(Alloc lt e)) -> Borrow lt qual p

    other -> other

ltCheck :: Term -> LifetimeCtx -> ThrowsError (Lifetime, LifetimeCtx)
ltCheck term ctx = case term of
    (Var name) -> do
        ltval <- getFromCtx name ctx
        return (ltval, ctx)

    v@(Lit _) -> return (LTDummy, ctx)

    (App t1 t2) -> do
        (_, ctx') <- ltCheck t1 ctx
        ltCheck t2 ctx'

    (Borrow lt q term) -> return (lt, ctx)

    (Lam funcLt ltParam var ty body retLt clos) -> do
        ctx' <- extend ltParam funcLt ctx
        let ctx'' = addToContext var funcLt ctx'
        (bodyLt, _) <- ltCheck body ctx''
        ltIsLT <- lifetimeLT retLt bodyLt ctx''
        unless ltIsLT $ throwError ErrLifetimeViolation
        return (LTDummy, ctx)

    other -> error (show other)

runLt :: Term -> ThrowsError Lifetime
runLt term = fmap fst $ ltCheck walked emptyCtx
    where walked = ltWalk term
