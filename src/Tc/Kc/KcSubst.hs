
module Tc.Kc.KcSubst where

import Data.List (union)
import Language.Haskell.Exts

import Utils.ErrMsg

-- this module defines some operations used
-- by the unification over kinds.

-- a synonym for kind variables and substituions

type KVar = Name

type KindSubst = [(KVar, Kind)]

nullSubst :: KindSubst
nullSubst = []

(@@) :: KindSubst -> KindSubst -> KindSubst
s1 @@ s2 = [(u, apply s1 k) | (u, k) <- s2] ++ s1

-- a type class for some operations over
-- things that have kinds

class Kinds a where
   vars  :: a -> [KVar]
   apply :: KindSubst -> a -> a

instance Kinds a => Kinds [a] where
   vars    = foldr (union . vars) []
   apply s = map (apply s)

instance Kinds Kind where
   vars KindStar      = []
   vars (KindFn l r)  = vars l `union` vars r
   vars (KindParen k) = vars k
   vars (KindVar v)   = [v]
   vars x             = unsupportedKindErrMsg x

   apply s KindStar      = KindStar
   apply s (KindFn l r)  = KindFn (apply s l)
                                  (apply s r)
   apply s (KindParen k) = KindParen (apply s k)
   apply s k@(KindVar v) = maybe k id (lookup v s)
   apply s x             = unsupportedKindErrMsg x
