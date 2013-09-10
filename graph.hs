module Graph where

{-data color = Color Int
           | Clear deriving Show
data vertex = Vertex Int color deriving Show
data edge = Edge vertex vertex deriving Show
data graph = Graph vertex [edges] deriving Show-}

import qualified Data.Graph as Graph


isChrodal :: Graph -> Bool
isChordal g = True

cycle :: Graph ->  [[ Vertex ]]
cycle g = [[1]]

{-
import Data.List
import qualified Data.Set as Set

type Vertex = Int
type Edge = (Int,Int)
type Edges = [Edge]
type Graph = [[ Vertex ]]

{-reduce :: ((a,a)->a)->a->[a]->a
reduce f b ([]) = b
reduce f b (x:[]) = f (b,x)
reduce f b (x:y:xs) = 
-}


{-
Graph - adjacency list,
buildgraph, takes in list of vertices, and list of edges and builds graph
list of edges may contain both (1,2) and (2,1) for an edge
-}
             
buildgraph :: [Vertex] -> Edges -> Graph
buildgraph v e = let e' = union (e) (map (\(x,y)->(y,x)) e)
                     e'' = tabulate (\n -> filter (\(x,y) -> x==n) e') (length v)
                 in
                  map (map (\(x,y)->y)) e''

-- maxV : Returns the max numbered vertex in of an Edge
maxV :: Edge -> Int
maxV (x,y) = if x > y then x else y

-- tabulate : creates a list of length n applying f to each index, given a
--            function f and integer n
tabulate :: (Vertex -> a) -> Vertex -> [a]
tabulate f n = map f [0..(n-1)]

-- isChordal : Returns true if the graph is chordal of not
isChordal :: Graph -> Bool
isChordal g = True

-- cycle : Compute a list of cycles in the graph (each cycle is the represented
--         as a list containing the vertices in the cycle) 
cycle :: Graph -> [[ Vertex ]]
cycle g = tabulate (\x -> dfs g x) (length g)

-- dfs : Given a graph and a vertex, run DFS on the graph and return the visited
--       vertices in a list
dfs :: Graph -> Vertex -> [ Vertex ]
dfs g v = [v]

-- nghbr : Given a graph and a vertex, return a list of the neighboring vertices
nghbr :: Graph -> Vertex -> [ Vertex ]
nghbr g v =  g !! v
-}
