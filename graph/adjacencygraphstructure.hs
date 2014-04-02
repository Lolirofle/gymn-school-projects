module AdjacencyGraph where

import Data.List
import Data.Ord
import Data.Maybe
import Data.Tuple

deleteFirst :: (a -> Bool) -> [a] -> [a]
deleteFirst _ []      = []
deleteFirst eq (y:ys) = if eq y then ys else y : deleteFirst eq ys

--TODO: Is it more efficient to use a map to represent the graph vertex?

-- |Graph Edge structure. Represents the edges of a graph
data GraphVertex a = GraphVertex a [a] deriving (Eq)

instance (Show a) => Show (GraphVertex a) where
	show (GraphVertex label neighbors) = (show label) ++ ":[" ++ (show neighbors) ++ "]"

getLabel :: GraphVertex a -> a
getLabel (GraphVertex v _) = v

getNeighbors :: GraphVertex a -> [a]
getNeighbors (GraphVertex _ e) = e

type AdjacencyGraph a = [GraphVertex a]

-- |Size of graph (Number of edges)
size :: AdjacencyGraph a -> Int
size graph = sum (map (length . getNeighbors) graph)

-- |Edges in the graph
edges :: AdjacencyGraph a -> [(a,a)]
edges = concatMap (\(GraphVertex label neighbors) -> map (\edgeToLabel -> (label,edgeToLabel)) neighbors)

-- |Vertices in the graph
vertices :: Eq a => AdjacencyGraph a -> [a]
vertices graph = map getLabel graph

-- |Order of graph (Number of vertices)
order :: AdjacencyGraph a -> Int
order = length

vertexWithLabel :: Eq a => a -> AdjacencyGraph a -> Maybe (GraphVertex a)
vertexWithLabel label graph = find (((==) label) . getLabel) graph

-- |Graph with specific vertex added
withVertex :: Eq a => GraphVertex a -> AdjacencyGraph a -> AdjacencyGraph a
withVertex vertex@(GraphVertex label neighbors) graph = case vertexWithLabel label graph of
	Just existingVertex@(GraphVertex _ existingNeighbors) -> (GraphVertex label (union existingNeighbors neighbors)) : (delete existingVertex graph)
	Nothing -> vertex:graph

-- |Graph with specific vertex removed
withoutVertex :: Eq a => a -> AdjacencyGraph a -> AdjacencyGraph a
withoutVertex label graph = deleteFirst (((==) label) . getLabel) updatedGraph
	where updatedGraph = map (\oldVertex@(GraphVertex l oldNeighbors) -> GraphVertex l (delete label oldNeighbors)) graph

toLabelsFrom :: Eq a => a -> AdjacencyGraph a -> [a]
toLabelsFrom label graph = case vertexWithLabel label graph of
	Just (GraphVertex _ neighbors) -> neighbors
	Nothing     -> []

fromLabelsTo :: Eq a => a -> AdjacencyGraph a -> [a]
fromLabelsTo label graph = foldl f [] graph
	where f fromLabels (GraphVertex from neighbors) =
		if elem label neighbors then
			from : fromLabels
		else
			fromLabels

main :: IO ()
main = do
	putStrLn $ show graph
	putStrLn $ "Without C: "  ++ (show $ withoutVertex 'C' graph)
	putStrLn $ "With C: "     ++ (show $ withVertex (GraphVertex 'C' "FEA") graph)
	putStrLn $ "Edges: "      ++ (show $ edges graph)
	putStrLn $ "Size: "       ++ (show $ size graph)
	putStrLn $ "Order: "      ++ (show $ order graph)
	putStrLn $ "Vertices: "   ++ (show $ vertices graph)
	putStrLn $ "To labels from B: " ++ (show $ toLabelsFrom 'B' graph)
	putStrLn $ "From labels to G: " ++ (show $ fromLabelsTo 'G' graph)
	where
		graph = 
			GraphVertex 'A' ['B'] : 
			GraphVertex 'B' ['C','D'] : 
			GraphVertex 'C' ['E'] : 
			GraphVertex 'D' ['F'] : 
			GraphVertex 'E' ['G'] : 
			GraphVertex 'F' ['G'] : 
			GraphVertex 'G' [] : 

			[]
