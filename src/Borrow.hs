module Borrow where

import Control.Monad.State
import qualified Data.IntMap.Strict as I

import Syntax

type LoanHeap = I.IntMap Loan

-- The borrow checker works by analyzing each borrow expression like `&t'. For
-- each borrow expression a `Loan' is computed, which is a data structure that
-- records (1) The value being borrowed; (2) the mutability and scope of the 
-- borrow; (3) and a set of restrictions

data Loan = Loan
    { lifetime :: Lifetime
    , qualifier :: Qualifier
    , restrictions :: [Restriction]
    }

type Restriction = (Location, [Action])

data Action = Mutate | Claim | Freeze
    deriving (Show, Eq)

data Borrowinfo = Borrowinfo
    { loanHeap :: LoanHeap
    , available :: [Location]
    }

type Borrowcheck = StateT Borrowinfo ThrowsError

allocateLoan :: Loan -> Borrowcheck Location
allocateLoan borrow = do
    info@(Borrowinfo lh (idx:rest)) <- get
    let newStore = I.insert idx borrow lh
    put info { loanHeap = newStore, available = rest }
    return idx

-- The kinds of expressions which in-scope loans can render illegal are:
-- - *assignments* (`lv = v`): illegal if there is an in-scope restriction
--   against mutating `lv`;
-- - *moves*: illegal if there is any in-scope restriction on `lv` at all;
-- - *mutable borrows* (`&mut lv`): illegal there is an in-scope restriction
--   against claiming `lv`;
-- - *immutable borrows* (`&lv`): illegal there is an in-scope restriction
--   against freezing `lv`.

locMatch :: Location -> [Restriction] -> [Action]
locMatch loc rs = concat . map snd $ filter (\(l, as) -> l == loc) rs

checkRestrictions :: Action -> Location -> LoanHeap -> ThrowsError Bool
checkRestrictions action loc lheap = do
    loan <- fromHeap loc lheap
    let rs = restrictions loan
    return $ not . any (== action) $ locMatch loc rs

canAssign :: Location -> LoanHeap -> ThrowsError Bool
canAssign = checkRestrictions Mutate

canMove :: Location -> LoanHeap -> ThrowsError Bool
canMove loc lheap = do
    loan <- fromHeap loc lheap
    return $ null (restrictions loan)

canMutBorrow :: Location -> LoanHeap -> ThrowsError Bool
canMutBorrow = checkRestrictions Claim

canImmBorrow :: Location -> LoanHeap -> ThrowsError Bool
canImmBorrow = checkRestrictions Freeze

borrowCheck = undefined
-- 
-- borrowCheck :: Term -> LocationCtx -> Borrowcheck ()
-- borrowCheck term ctx = case term of
--     (Var name)
-- 
runBorrow :: Term -> Borrowcheck ()
runBorrow term = borrowCheck term emptyCtx
