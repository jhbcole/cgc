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
type Edge = (Int,Int)
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
maxV :: Edge -> Int
maxV (x,y) = if x > y then x else y

-- tabulate : creates a list of length n applying f to each index, given a
--            function f and integer n
tabulate :: (Vertex -> a) -> Vertex -> [a]
tabulate f n = map f [0..(n-1)]

-- nghbr : Given a graph and a vertex, return a list of the neighboring vertices
nghbr :: Graph -> Vertex -> [ Vertex ]
nghbr g v = g Map.! v
            --in if Mb.isJust l then Mb.fromJust l else []

isnghbr :: Graph -> Vertex -> Vertex -> Bool
isnghbr g v1 v2 = v2 `elem` (nghbr g v1)

seo :: Graph -> [ Vertex ]
seo g = let verts = tabulate (\x -> x) (Map.size g)
            weights = PQ.fromAscList (map (\x -> (0,x)) verts) 
        in
         List.reverse (seo' g weights [])

seo' :: Graph -> PQ.MaxHeap (Vertex,Vertex) -> [ Vertex ] -> [ Vertex ] 
seo' g weights l =
  case PQ.view weights of
    Nothing -> l
    Just ((_, v), weights') ->
      let
        (left, right) = PQ.partition (\(_, v') -> isnghbr g v v'{- is neighbor of v -}) weights'
        left' = PQ.fromList (map (\(prio, val) -> (prio + 1, val)) (PQ.toList left))
        w = PQ.union left' right
     in seo' g w (v : l)
      
       
