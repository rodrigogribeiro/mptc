module Tc.TcSimplify where

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Data.List

import Language.Haskell.Exts

import Tc.Class
import Tc.TcAlphaEq
import Tc.TcCriteria
import Tc.TcMonad
import Tc.TcOrdering
import Tc.TcSubst

import Utils.ErrMsg
import Utils.EnvMonad
import Utils.Id

-- this module have functions for doing the context
-- reduction of haskell types.

-- context reduction

reduce :: Context -> TcM Context
reduce ps 
    = do
		-- first, try to reduce using instances
		ps1 <- toHnfs ps 
		-- second, try to reduce using super class information
		ps2 <- mapM reduceBySuper ps1
		let 
			pss = zip ps1 ps2
			f (c,cs) ac = if null cs then c : ac else simplify (cs ++ ac)
		return (nub $ foldr f [] pss)


-- reducing by super classes info

reduceBySuper :: Asst -> TcM [Asst]
reduceBySuper pi
	= do
		ps <- liftM supers (getClass (toId $ className pi))
		ps1 <- liftM concat (mapM reduceBySuper ps)
		return (simplify (map (updateTypes (types pi)) ps1))

simplify :: [Asst] -> [Asst]
simplify 
	= loop []
	  where
		loop rs [] = rs
		loop rs (p : ps) 
			| p `elem` ps = loop rs ps
			| otherwise = loop (p : rs) ps


-- converting to a head normal form

toHnfs :: Context -> TcM Context
toHnfs ps 
	= do
		phis <- mapM (\_ -> initialPhi) ps
		pss <- zipWithM toHnf phis ps
		return (concat pss)

toHnf :: Phi -> Asst -> TcM [Asst]
toHnf phi p = simplByInst phi p
--	| isHnf p 
--		= return [p]
--	| otherwise 
--		= simplByInst phi p

simplByInst :: Phi -> Asst -> TcM [Asst]
simplByInst phi pi
	= do
--		liftIO (putStrLn $ "Trying to reduce " ++ prettyPrint pi)
		delta <- matches pi
		case delta of
			[] -> return [pi]
			[(p,pi')] 
				-> do
					phi' <- updatePhi pi' pi phi
					liftM concat (mapM (toHnf phi') p)
			pis -> overlappingInstancesError pis
 

-- head normal form predicate

isHnf :: Asst -> Bool
isHnf (ClassA _ ts) = all hnf ts
isHnf (InfixA l _ r) = all hnf [l,r]
isHnf x = unsupportedDeclMsg x

hnf :: Type -> Bool
hnf (TyVar _) = True
hnf (TyApp l _) = hnf l
hnf (TyParen t) = hnf t
hnf _ = False


-- matches function

matches :: Asst -> TcM [(Context, Asst)]
matches pi 
	= do
		is <- instsFrom pi
		liftM catMaybes (mapM (gen pi) is)

gen :: Asst -> Inst -> TcM (Maybe (Context, Asst))
gen pi i 
	= do
		let pi' = toAsst i
		pi1 <- liftM toAsst (quantifyInst i)
		s <- defaultHandle (matchfy1 False pi' pi)
		return (maybe Nothing (\s' -> Just (apply s' (instsupers i), pi1)) s)

