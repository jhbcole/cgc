module Graph where

{-data color = Color Int
           | Clear deriving Show
data vertex = Vertex Int color deriving Show
data edge = Edge vertex vertex deriving Show
data graph = Graph vertex [edges] deriving Show-}

import Data.List

type Graph = [[ Int ]]

{-reduce :: ((a,a)->a)->a->[a]->a
reduce f b ([]) = b
reduce f b (x:[]) = f (b,x)
reduce f b (x:y:xs) = 
-}
             
buildgraph :: [(Int,Int)]  -> Graph
buildgraph e = let maxl = map (\(x,y) -> if x > y then x else y) e
                   max = foldl1 (\x y -> if x > y then x else y) (maxl)
                   e' = union (e) (map (\(x,y)->(y,x)) e)
                   e'' = tabulate (\n -> filter (\(x,y) -> x==n) e') max
               in
                map (map (\(x,y)->y)) e''
                
max :: (Int, Int) -> Int
max (x,y) = if x > y then x else y

tabulate :: (Int -> a) -> Int -> [a]
tabulate f n = map f [1..n]
