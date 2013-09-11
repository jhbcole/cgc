module Graph where

{-data color = Color Int
           | Clear deriving Show
data vertex = Vertex Int color deriving Show
data edge = Edge vertex vertex deriving Show
data graph = Graph vertex [edges] deriving Show-}


import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Graph as Graph
import qualified Data.Heap as PQ
import qualified Data.Maybe as Mb

type Vertex = Int
type Edge = (Vertex,Vertex)
type Edges = [Edge]
type Graph = Map.Map Vertex [Vertex]

{-
Graph - function from Vertex to list of Vertices.
buildgraph, takes in list of vertices, and list of edges and builds graph
list of edges may contain both (1,2) and (2,1) for an edge
-}
             
buildgraph :: [Vertex] -> Edges -> Graph
buildgraph v e = let e' = List.union (e) (map (\(x,y)->(y,x)) e)
                     e'' = tabulate (\n -> filter (\(x,y) -> x==n) e') (length v)
                     e''' = (map (map (\(x,y)->y)) e'')
                 in
                  Map.fromList(tabulate (\x -> (x,(e''' !! x))) (length(e''')))

-- maxV : Returns the max numbered vertex in of an Edge
maxV :: Edge -> Vertex
maxV (x,y) = if x > y then x else y

-- tabulate : creates a list of length n applying f to each index, given a
--            function f and integer n
tabulate :: (Int -> a) -> Int -> [a]
tabulate f n = map f [0..(n-1)]

-- nghbr : Given a graph and a vertex, return a list of the neighboring vertices
nghbr :: Graph -> Vertex -> [ Vertex ]
nghbr g v = g Map.! v
            --in if Mb.isJust l then Mb.fromJust l else []

isnghbr :: Graph -> Vertex -> Vertex -> Bool
isnghbr g v1 v2 = v2 `elem` (nghbr g v1)

-- Simplicial Elimination Ordering
seo :: Graph -> [ Vertex ]
seo g = let verts = tabulate (\x -> x) (Map.size g)
            weights = PQ.fromAscList (map (\x -> (0,x)) verts) 
        in
         List.reverse (seo' g weights [])

seo' :: Graph -> PQ.MaxHeap (Vertex, Vertex) -> [ Vertex ] -> [ Vertex ]
seo' g weights l =
  case PQ.view weights of
    Nothing -> l
    Just ((_, v), weights') ->
      let
        (left, right) = PQ.partition (\(_, v') -> isnghbr g v v'{- is neighbor of v -}) weights'
        left' = PQ.fromList (map (\(prio, val) -> (prio + 1, val)) (PQ.toList left))
        w = PQ.union left' right
     in seo' g w (v : l)

-- Greedy coloring algorithm, takes a graph, outputs a list of tuples
-- Vertex paired with Int, which represents color
coloring :: Graph -> [(Vertex, Int)]
coloring g = let m = Map.map (\x-> -1) g
                 s = seo g
              -- c = List.map(\v -> (List.map(\x -> m List.!! x)(g List.!! v))) m
             in
               color g m s

-- Color graph
color :: Graph -> Map.Map Vertex Int -> [ Vertex ] -> [(Vertex,Int)]
color g m [] = Map.toAscList m
color g m s = let n = nghbr g (List.head s)
                  n' = List.map(\x -> m Map.! x) n
                  m' = Map.insert (List.head s) (mex n') m
              in
               color g m' (tail s)

--Finds the Minimally Excluded Element of a list
mex :: [ Vertex ] -> Vertex
mex [] = 0
mex l = List.minimum([0..((List.maximum l)+2)] List.\\ l)

