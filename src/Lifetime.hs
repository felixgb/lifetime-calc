module Lifetime where

import Syntax

lifetimeOrd :: Lifetime -> Lifetime -> LifetimeCtx -> ThrowsError Bool
lifetimeOrd a b ctx = do
    (LifetimeLit n1) <- lookupIfVar a ctx
    (LifetimeLit n2) <- lookupIfVar b ctx
    return $ n1 < n2

-- populate dummy borrows
ltWalk :: Term -> Term
ltWalk term = case term of
    (App e1 e2) -> App (ltWalk e1) (ltWalk e2)

    (Seq ts) -> Seq (map ltWalk ts)

    (Lam lt name ty body fnLt clos) -> Lam lt name ty (ltWalk body) fnLt clos

    (Borrow _ qual p@(Pointer lt loc)) -> Borrow lt qual p

    other -> other

ltCheck :: Term -> LifetimeCtx -> ThrowsError (Term, LifetimeCtx)
ltCheck term ctx = case term of
    (Var name) = do
        val <- getFromCtx name ctx
        ltCheck val ctx

    v@(Lit _) -> return (v, ctx)

    (App t1 t2) -> do
        (t1', ctx') <- ltCheck t1 ctx
        ltCheck t2 ctx'
        
