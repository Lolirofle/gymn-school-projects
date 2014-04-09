module AdjacencyGraph where

import qualified Graph
import qualified ListUtil
import Data.List
import Data.Ord
import Data.Maybe
import Data.Tuple
import Data.Function (on)

--TODO: Is it more efficient to use a map to represent the graph vertex? Consider using indexes for the vertices with a independent label instead of using the label as a form of identification

-- |Graph Edge structure. Represents the edges of a graph
data VertexContainer a = VertexContainer{
	vertexId        :: Int,
	vertexLabel     :: a,
	vertexNeighbors :: [a]
} deriving (Eq)

instance (Show a) => Show (VertexContainer a) where
	show (VertexContainer _ label neighbors) = (show label) ++ ":[" ++ (show neighbors) ++ "]"

data Graph a = Graph [VertexContainer a] deriving (Eq,Show)

instance Graph.Graph Graph where
	empty = Graph []

	isEmpty (Graph vertices) = null vertices

	-- |Size of graph (Number of edges/connections)
	size (Graph vertices) = sum (map (length . vertexNeighbors) vertices)

	-- |Edges in the graph
	edges (Graph vertices) = concatMap (\(VertexContainer _ label neighbors) -> map (\edgeToLabel -> (label,edgeToLabel)) neighbors) vertices

	-- |Vertices in the graph
	vertices (Graph vertices) = map vertexLabel vertices

	-- |Order of graph (Number of vertices/nodes)
	order (Graph vertices) = length vertices

	toLabelsFrom label graph = case vertexWithLabel label graph of
		Just (VertexContainer _ _ neighbors) -> neighbors
		Nothing     -> []

	hasEdge from to graph = case vertexWithLabel from graph of
		Just vertex -> elem to (vertexNeighbors vertex)
		Nothing       -> False

	--TODO: Needs better implementation. (order graph) makes it having higher complexity
	withVertex label graph@(Graph vertices) = Graph (ListUtil.addUniqueOf ((==) `on` vertexLabel) (VertexContainer (Graph.order graph) label []) vertices)

	withoutVertex label (Graph vertices) = Graph (ListUtil.filterFirst (((==) label) . vertexLabel) updatedGraph)
		where updatedGraph = map (\oldVertex@(VertexContainer i l oldNeighbors) -> VertexContainer i l (delete label oldNeighbors)) vertices

	withEdge from to graph@(Graph vertices) = Graph (map addEdgeToVertex vertices)
		where addEdgeToVertex vertex@(VertexContainer i label neighbors) = 
			if label == from then
				VertexContainer i label (ListUtil.addUnique to neighbors)
			else
				vertex

	withoutEdge from to graph@(Graph vertices) = Graph (map removeEdgeFromVertex vertices)
		where removeEdgeFromVertex vertex@(VertexContainer i label neighbors) = 
			if label == from then
				VertexContainer i label (ListUtil.filterFirst (==to) neighbors)
			else
				vertex


vertexWithLabel :: Eq a => a -> Graph a -> Maybe (VertexContainer a)
vertexWithLabel label (Graph vertices) = find (((==) label) . vertexLabel) vertices

withVertexWithEdges :: Eq a => a -> [a] -> Graph a -> Graph a
withVertexWithEdges label neighbors graph@(Graph vertices) = case vertexWithLabel label graph of
	Just existingVertex@(VertexContainer i _ existingNeighbors) -> Graph ((VertexContainer i label (union existingNeighbors neighbors)) : (delete existingVertex vertices))
	Nothing -> Graph ((VertexContainer (Graph.order graph) label neighbors):vertices)

-- | Neighbors that `from` can reach to
fromLabelsTo :: Eq a => a -> Graph a -> [a]
fromLabelsTo label (Graph vertices) = foldl f [] vertices
	where f fromLabels (VertexContainer _ from neighbors) =
		if elem label neighbors then
			from : fromLabels
		else
			fromLabels

main :: IO ()
main = do
	putStrLn $ show graph
{--	putStrLn $ "Without C: "  ++ (show $ withoutVertex 'C' graph)
	putStrLn $ "With' C: "     ++ (show $ withVertex' (VertexContainer 'C' "FEA") graph)
	putStrLn $ "With D: "     ++ (show $ withVertex 'D' graph)
	putStrLn $ "With A to C: "++ (show $ withEdge 'A' 'C' graph)
	putStrLn $ "Without A to B: "++ (show $ withoutEdge 'A' 'B' graph)
	putStrLn $ "Edges: "      ++ (show $ Graph.edges graph)
	putStrLn $ "Size: "       ++ (show $ Graph.size graph)
	putStrLn $ "Order: "      ++ (show $ Graph.order graph)
	putStrLn $ "Vertices: "   ++ (show $ Graph.vertices graph)
--}	putStrLn $ "To labels from B: " ++ (show $ Graph.toLabelsFrom 'B' graph)
	putStrLn $ "From labels to G: " ++ (show $ fromLabelsTo 'G' graph)
	putStrLn $ "Has edge A to B: "  ++ (show $ Graph.hasEdge 'A' 'B' graph)
	putStrLn $ "Walk using head: "  ++ (show $ Graph.walk' (Just . head) 'A' graph)
	putStrLn $ "With C -> [A,B]: "  ++ (show $ withVertexWithEdges 'B' "ABC" graph)
	where
		graph = Graph (
			VertexContainer 0 'A' ['B'] : 
			VertexContainer 1 'B' ['C'] : 
			VertexContainer 2 'C' [] : 

			[])
