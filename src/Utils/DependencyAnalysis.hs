module Utils.DependencyAnalysis(dependencyAnalysis, AnalysisTy(..)) where

import Control.Monad.Reader
import Data.Array ((!))
import Data.Graph
import qualified Data.IntMap as IMap
import Data.List (intersect, nub)
import qualified Data.Map as Map
import Data.Maybe (fromJust, catMaybes)
import Data.Tree (flatten)

import Utils.Id
import Utils.Env
import Utils.Nameable
import Utils.Debug


-- this module implements the dependency analysis algorithm
-- specified in SPJ book.

-- first the definition of a data type for the analysis type

data AnalysisTy = KindAnalysis | TypeAnalysis 
                  deriving (Eq, Ord, Show)
                  
                  
dependencyAnalysis :: (Show a, Nameable a, Referenceable a) => AnalysisTy -> [a] -> [[a]]
dependencyAnalysis ty ds
        = runReader (analysis ty ds) table
          where
             table = mkIdTable ds
             
-- the analysis algorithm is done within a reader monad

type AnalysisM a = Reader (Env Vertex) a

analysis :: (Nameable a, Referenceable a) => AnalysisTy -> [a] -> AnalysisM [[a]]
analysis ty ds = do
                    graph <- dependencyGraph ty ds
                    (cgraph,table) <- coalesce graph
                    finish (topSort cgraph) table ds

-- finishing the dependency analysis

finish :: Nameable a => [Vertex] -> IMap.IntMap [Vertex] -> [a] -> AnalysisM [[a]]
finish vs m ds 
    = do
        env <- asks reverseEnv
        let 
            table = foldr (\d ac -> insert (name d) d ac) empty ds
            vss = map (\v -> fromJust $ IMap.lookup v m) vs
            f = map (fromJust . flip Map.lookup env) 
            g = map (fromJust . flip Map.lookup table)
        return (reverse (map (g . f) vss))

-- creating a map from id's to decls

mkIdTable :: Nameable a => [a] -> Env Vertex
mkIdTable = fst . foldr (step . name) (empty, 1) 
            where
               step d (ac,n) = insert' d n ac
               insert' n' v m = if member n' m then (m,v) 
                                  else (insert n' v m, v + 1)

-- creating the dependency graph

dependencyGraph :: (Nameable a, Referenceable a) => AnalysisTy -> [a] -> AnalysisM Graph
dependencyGraph ty ds 
    = do
        let 
            -- here it was necessary to force strict evaluation 
            -- of the list of id's
            ds' = map (\d -> let ts = referencedNames ty d ns in (name d, ts)) ds
            ns  = map name ds
        n <- asks size
        edges <- createEdges ds'
        return (buildG (1,n) edges)

-- creating the dependency edges for the graph

createEdges :: [(Id,[Id])] -> AnalysisM [Edge]
createEdges is = foldM step [] is
              where
                f x = asks (lookupEnv x)
                step ac (d,ds) 
                    = do
                        -- here is safe to use fromJust
                        source <- liftM fromJust (f d)
                        targets <- liftM catMaybes (mapM f ds)
                        return (foldr (\d' ac' -> (source, d') : ac') ac targets)

-- coalescing the dependency graph

coalesce :: Graph -> AnalysisM (Graph, IMap.IntMap [Vertex]) 
coalesce g = do
                let
                  n = length ns
                  ns = map flatten (scc g)
                  table = Map.fromList (zip ns [1..])
                  table' = IMap.fromList (zip [1..n] ns)
                  reach xs ys = any (\x -> not $ null ((g ! x) `intersect` ys)) xs 
                  f (x, y) = (g' x, g' y)
                  g' x =  fromJust (Map.lookup x table)
                  es = [(x,y) | x <- ns, y <- ns, reach x y, x /= y]
                  es' = map f es
                return (buildG (1,n) es', table')

-- some auxiliar functions

reverseEnv = foldrWithId revins Map.empty
             where
                revins k v ac = Map.insert v k ac   
                
referencedNames KindAnalysis d ns = kindRefNames d `intersect` ns
referencedNames TypeAnalysis d ns = typeRefNames d `intersect` ns         