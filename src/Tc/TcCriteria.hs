module Tc.TcCriteria where

import Data.List (union,intercalate, nub)
import qualified Data.Map as Map
import qualified Data.IntMap as IMap

import Language.Haskell.Exts

import Tc.TcAlphaEq
import Tc.Class
import Tc.TcEnv
import Tc.TcMonad
import Tc.TcSubst

import Utils.EnvMonad
import Utils.ErrMsg
import Utils.Id

-- implementation of the termination criteria
-- used in satisfiability and context reduction

-- some type definitions used by the termination criteria

type Phi = Map.Map Asst ([Int],Pi)
type Pi = [Asst]

-- initial value of \Phi function

initialPhi :: TcM Phi
initialPhi 
	= do
		is <- liftM (map toAsst) allInsts
		let 
			step i ac = Map.insert i (initSize (length (types i)), []) ac
			initSize n = replicate (n + 1) (maxBound :: Int)
		return (foldr step Map.empty is)
				

-- updating phi function

updatePhi :: Asst -> Asst -> Phi -> TcM Phi
updatePhi pi0 pi phi
	= do
		let
			n' = eta pi
			(vs,pI) = maybe (error ("Panic! TcCriteria.updatePhi! " ++ prettyPrint pi0)) id 
					        (Map.lookup pi0 phi)
			n = head vs
			error_found = all (== flag) vs
--		liftIO (putStrLn $ "vs" ++ show vs)
--		liftIO (putStrLn $ "Eta - pi:" ++ show n')
--		liftIO (putStrLn $ "v0:" ++ show n)
		when error_found (notDeacreasingError pi)
		cond <- liftM not (pi `inPi` pI)
--		liftIO (putStrLn $ "Pi:" ++ intercalate "," (map prettyPrint pI))
--		liftIO (print cond)
		if n > n' then return (Map.insert pi0 (n' : tail vs, pI) phi)
		   else if n == n' && cond then ---liftIO (putStrLn $ "Pi:" ++ intercalate "," (map prettyPrint (pi:pI))) >>
											return (Map.insert pi0 (vs, pi : pI) phi)
			  		else if n == n' then notDeacreasingError pi
							else
								do
									let 
										vs' = map eta (types pi)
										vs1 = tail vs
										vsf = flag : zipWith step vs1 vs' 
										step a b = if b < a then b else flag
									--liftIO (putStrLn $ "I:" ++ show vsf)
									return (Map.insert pi0 (vsf, pI) phi)


inPi :: Asst -> [Asst] -> TcM Bool
pi `inPi` pI = anyM (alphaEqM pi) pI
		
				
-- an overloaded operation for calculating the \eta value of something

class Eta a where
	eta :: a -> Int 

instance Eta a => Eta [a] where
	eta = foldr ((+) . eta) 0

instance Eta Type where
	eta (TyFun l r) = eta [l,r]
	eta (TyTuple _ ts) = 1 + eta ts
	eta (TyList t) = 1 + eta t
	eta (TyApp l r) = eta [l,r]
	eta (TyVar _) = 1
	eta (TyCon _) = 1
	eta (TyParen t) = eta t
	eta (TyInfix l _ r) = 1 + eta [l,r]
	eta x = unsupportedDeclMsg x

instance Eta Asst where
	eta (ClassA _ ts) = eta ts
	eta (InfixA l _ r) = eta [l,r]
	eta x = unsupportedDeclMsg x

--helper functions

allInsts :: TcM [Inst]
allInsts 
	= gets (concatMap instances . Map.elems . classenv)

unifyOk :: Asst -> Asst -> TcM Bool
unifyOk pi pi'
	= handle (unify1 False pi pi') True False

className :: Asst -> QName
className (ClassA n _) = n
className (InfixA _ n _) = n        

types :: Asst -> [Type]  
types (ClassA _ ts) = ts
types (InfixA l _ r) = [l,r]

updateTypes :: [Type] -> Asst -> Asst
updateTypes ts (ClassA n _) = ClassA n ts
updateTypes [l,r] (InfixA _ n _) = InfixA l n r

instsFrom :: Asst -> TcM [Inst]
instsFrom = getInstances . toId . className   

toAsst :: Inst -> Asst
toAsst i = ClassA (unid $ instname i) (instparameters i)

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p []     = return False
anyM p (x:xs) = liftM2 (||) (p x) (anyM p xs)

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p []     = return True
allM p (x:xs) = liftM2 (&&) (p x) (allM p xs)

flag = -1

