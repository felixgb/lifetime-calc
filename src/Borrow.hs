module Borrow where

import Control.Monad.State

import Syntax

isImm :: Qualifier -> Bool
isImm Mut = False
isImm Imm = True

holeInit :: Type -> Shadow
holeInit (TyPointer lt ty) = SPointer $ holeInit ty
holeInit (TyBorrow l q ty) = SBorrow q $ holeInit ty
holeInit other = SInit
-- 
-- data Borrowinfo = Borrowinfo
--     { shadowHeap ::
--     }
-- 
-- type Borrowcheck = StateT Borrowinfo ThrowsError
-- 
-- shallow :: Name -> Shadow -> Borrowcheck ()
-- shallow = undefined

