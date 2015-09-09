module Utils.Env where

-- this module defines a generic enviroment 
-- for the inference process

import qualified Data.Map as Map

import Utils.Id 

-- in a environment, a Id is always the key

type Env a = Map.Map Id a

-- creating an empty environment

empty :: Env a
empty = Map.empty


-- inserting a value in the environment

insert :: Id -> a -> Env a -> Env a
insert i v env = Map.insert i v env

-- looking up a value in a environemnt, the function
-- is called lookupEnv just to avoid name clashes with
-- Prelude.lookup

lookupEnv :: Id -> Env a -> Maybe a
lookupEnv i = Map.lookup i

-- a map over an environment

mapEnv :: (a -> b) -> Env a -> Env b
mapEnv f = Map.map f 


-- getting the domain of a environment

domain :: Env a -> [Id]
domain = Map.keys


-- getting the image of a environment

image :: Env a -> [a]
image = Map.elems

-- removing some entries with specified Id's

(^-) :: Env a -> [Id] -> Env a
env ^- ids = foldr Map.delete env ids

-- projection on a set of ids

(|:) :: Env a -> [Id] -> Env a
env |: ids = Map.filterWithKey (\n _ -> n `elem` ids) env

-- unionl preserves entries in left

unionl :: Env a -> Env a -> Env a
unionl env1 env2 = Map.union env1 env2

-- unionr preserves entries in right

unionr :: Env a -> Env a -> Env a
unionr env1 env2 = Map.union env2 env1

-- unionc preserves entries in left and right. 
-- Allows conflict entries

unionc :: Env a -> Env a -> Env [a]
unionc env1 env2 = Map.unionWith (++) env1' env2'
                                        where
                                                env1' = Map.map wrap env1
                                                env2' = Map.map wrap env2
                                                wrap x = [x]

-- filtering a environment based on the type of Id

unquals :: Env a -> Env a
unquals = Map.filterWithKey (\k _ -> isunqual k)

quals :: Env a -> Env a
quals = Map.filterWithKey (\k _ -> isqual k)

-- getting a environment without duplicated entries

justSingles :: Env [a] -> Env a
justSingles envs = Map.foldrWithKey step Map.empty envs
                                where
                                        p [_] = True
                                        p _   = False                   
                                        step k v ac 
                                                | p v = Map.insert k (head v) ac 
                                                | otherwise = ac

-- a foldr over environments

foldrWithId :: (Id -> a -> b -> b) -> b -> Env a -> b
foldrWithId f v env = Map.foldrWithKey f v env

-- building from a list

fromList :: [(Id, a)] -> Env a
fromList = Map.fromList

size ::Env a -> Int
size = Map.size

member :: Id -> Env a -> Bool
member i = Map.member i