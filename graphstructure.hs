module Graph where

import System.Environment
import System.IO
import Data.List
import Data.Ord
import Data.Maybe
import Data.Tuple
import qualified Data.Map as Map
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary.Put
import qualified Data.Binary.Get as Binary.Get
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Char (ord,chr)

-- |Graph Edge structure. Represents the edges of a graph
data GraphEdge a cost = GraphEdge a a cost

getFrom (GraphEdge from _ _) = from 
getTo   (GraphEdge _ to _)   = to
getCost (GraphEdge _ _ cost) = cost

getFroms :: AdjacencyGraph a b -> [a]
getFroms = map getFrom

getTos   :: AdjacencyGraph a b -> [a]
getTos   = map getTo

getCosts :: AdjacencyGraph a b -> [b]
getCosts = map getCost

-- |Implementation of equality
instance Eq a => Eq (GraphEdge a b) where
	(GraphEdge x1 y1 _) == (GraphEdge x2 y2 _) = x1 == x2 && y1 == y2

-- |Implementation of compare. Ordered lexicographically
instance (Eq a,Ord a) => Ord (GraphEdge a b) where
	(GraphEdge x1 y1 _) `compare` (GraphEdge x2 y2 _) =
		if x1==x2 then y1 `compare` y2 else x1 `compare` x2

{-- |Implementation of string conversion
instance Show a => Show (GraphEdge a b) where
	show (GraphEdge x y _) = (show x) ++ " --> " ++ (show y)
--}
-- |Implementation of string conversion
instance (Show a,Show b) => Show (GraphEdge a b) where
	show (GraphEdge x y cost) = (show x) ++ " -(" ++ (show cost) ++ ")> " ++ (show y)

data GraphPath a b = GraphPath {path::[a],costs::[b]}
emptyPath = GraphPath {path = [],costs = []}

type AdjacencyGraph node cost = [GraphEdge node cost]

-- |Size of graph (Number of edges)
size :: AdjacencyGraph a b -> Int
size = length

-- |Edges in the graph
edges :: AdjacencyGraph a b -> AdjacencyGraph a b
edges = id

-- |Vertices in the graph
vertices :: Eq a => AdjacencyGraph a b -> [a]
vertices edges = foldl buildUnique [] edges where
	buildUnique result (GraphEdge from to _) = 
		(if (elem from result) then result2 else from:result2) where result2 = (if (elem to result) then result else to:result)

-- |Order of graph (Number of vertices)
order :: Eq a => AdjacencyGraph a b -> Int
order = length . vertices

-- |Graph with specific edge added
addEdge :: Eq a => GraphEdge a b -> AdjacencyGraph a b -> AdjacencyGraph a b
addEdge edge edges = if (elem edge edges) then edges else edge:edges

-- |Graph with specific edge removed
removeEdge :: Eq a => GraphEdge a b -> AdjacencyGraph a b -> AdjacencyGraph a b
removeEdge = delete

-- |The edges of a value
edgesOf :: Eq a => a -> AdjacencyGraph a b -> [GraphEdge a b]
edgesOf value edges = (filter (\(GraphEdge from to _) -> from == value || to == value) edges)

-- |The to values of a from value
toEdgesFrom :: Eq a => a -> AdjacencyGraph a b -> [GraphEdge a b]
toEdgesFrom value edges = filter (\(GraphEdge from _ _) -> from == value) edges

-- |The from values of a to value
fromEdgesTo :: Eq a => a -> AdjacencyGraph a b -> [GraphEdge a b]
fromEdgesTo value edges = filter (\(GraphEdge _ to _) -> to == value) edges

-- |The to values of a from value
toValuesFrom :: Eq a => a -> AdjacencyGraph a b -> [a]
toValuesFrom value edges = map getTo (toEdgesFrom value edges)

-- |The from values of a to value
fromValuesTo :: Eq a => a -> AdjacencyGraph a b -> [a]
fromValuesTo value edges = map getFrom (fromEdgesTo value edges)

-- |The path it walks when the `routeChooser` is applied at every intersection
walk :: Eq a => (AdjacencyGraph a b -> a) -> AdjacencyGraph a b -> a -> [a]
walk routeChooser graph start = if null routes then [start] else start:(walk routeChooser graph (routeChooser routes)) where
	routes = toEdgesFrom start graph

-- |The path it walks backwards when the `routeChooser` is applied at every intersection
walkBackwards :: Eq a => (AdjacencyGraph a b -> a) -> AdjacencyGraph a b -> a -> [a]
walkBackwards routeChooser graph start = if null routes then [start] else start:(walk routeChooser graph (routeChooser routes)) where
	routes = fromEdgesTo start graph


-- |The start vertices. No values point to these (High complexity because of nub and the elem in the filter?)
startValues :: Eq a => AdjacencyGraph a b -> [a]
startValues graph = filter (`notElem` (getTos graph)) (nub $ (getFroms graph))

-- |The end vertices. No values start these (High complexity because of nub and the elem in the filter?)
endValues :: Eq a => AdjacencyGraph a b -> [a]
endValues graph = filter (`notElem` (getFroms graph)) (nub $ (getTos graph))

-- |The connected vertices that is neither a start or end vertex.
connectedValues :: Eq a => AdjacencyGraph a b -> [a]
connectedValues graph = nub (uncurry intersect (unzip (map (\edge -> (getFrom edge,getTo edge)) graph)))

getEdge :: Eq a => a -> a -> AdjacencyGraph a b -> Maybe (GraphEdge a b)
getEdge from to graph = find (\edge -> from == getFrom edge && to == getTo edge) graph

--TODO: This should behave differently if an undirected graph is implemented
isAdjacent :: Eq a => a -> a -> AdjacencyGraph a b -> Bool
isAdjacent a b graph = isJust (getEdge a b graph)

-- |Simplifies a graph by summing the costs and having as few intersections and paths as possible
--simplify :: (b -> b -> b) -> AdjacencyGraph a b -> AdjacencyGraph a b
--simplify costSumFunc graph = 

-- |Finds all paths from one value to another value
findPaths :: Eq a => a -> a -> AdjacencyGraph a b -> [[a]]
findPaths from to graph = findPaths' from to graph [] where
	findPaths' from to graph path = foldl f [] (fromValuesTo to graph) where
		f result fromValue =
			-- If already checked the `fromValue` before in the path, skip and avoid infinite loop
			if elem fromValue path then
				result
			-- If the searched from value is found, then it is at the beginning and should construct the list entry of the path
			else
				let newPath = to : path in
				if fromValue==from then
					(from : newPath) : result
				-- Else search for new paths to the beginning from the current `fromValue`
				else
					result ++ (findPaths' from fromValue graph (newPath))

-- Works with undirected graphs, but we have directed. Also, it may be able to be optimized if it knows that the graph already is spanned in the middle of the fold
-- TODO: Checking for loop correctly using a forest. It checks every tree in the forest when creating a new edge, looking for the to value. If it is found, then merge the two trees into one and do tis until there's one left. That's when the tree is finished and no more edges are neccessary to add.
minimumSpanning_kruskal :: (Eq a) => (GraphEdge a b -> GraphEdge a b -> Ordering) -> AdjacencyGraph a b -> AdjacencyGraph a b
minimumSpanning_kruskal compareFunc graph = foldl connected [] sorted where
	sorted = sortBy compareFunc graph
	connected result edge = 
		if isJust $ find (\e -> getTo edge == getFrom e || getFrom edge == getFrom e) result then
			result
		else
			edge : result

findPath_djikstra :: (Eq a,Num b,Ord b) => a -> a -> AdjacencyGraph a b -> [a]
findPath_djikstra from to graph = findPath_djikstra' from to graph (Map.singleton from 0)

findPath_djikstra' :: (Eq a,Num b,Ord b) => a -> a -> AdjacencyGraph a b -> Map.Map a b -> [a]
findPath_djikstra' from to graph boxed = 
	let adjacentToFrom = toEdgesFrom from graph
	    nextBox = getFrom $ minimumBy (comparing getCost) adjacentToFrom
	    nextBoxed = boxed
	in nextBox : []

exportData :: (a -> Binary.Word32) -> AdjacencyGraph a b -> Binary.Put
exportData valueToWord graph = mapM_ serializeEdge graph where
	serializeEdge edge = do
		Binary.Put.putWord32be (valueToWord (getFrom edge))
		Binary.Put.putWord32be (valueToWord (getTo edge))

importData :: (Binary.Word32 -> a) -> Binary.Get (GraphEdge a Int)
importData wordToValue = do
	from <- Binary.Get.getWord32be
	to <- Binary.Get.getWord32be
	return (GraphEdge (wordToValue from) (wordToValue to) 0)

main :: IO ()
main = do
{--	file <- openBinaryFile "djikstra_example_graph.dat" WriteMode
	ByteString.Lazy.hPut file (Binary.Put.runPut $ exportData (fromIntegral . ord) graph)
	hClose file
--}

	file <- openBinaryFile "djikstra_example_graph.dat" ReadMode
	rawData <- ByteString.Lazy.hGetContents file
	let importedGraph = Binary.Get.runGet (importData (chr . fromIntegral)) rawData
	print importedGraph
	hClose file

	putStrLn $ "Edges: "      ++ (show $ edges graph)
{--	putStrLn $ "Size: "       ++ (show $ size graph)
	putStrLn $ "Order: "      ++ (show $ order graph)
	putStrLn $ "Vertices: "   ++ (show $ vertices graph)
	putStrLn $ "Edges of 5: " ++ (show $ edgesOf 5 graph)
	putStrLn $ "To values from 1: " ++ (show $ toValuesFrom 1 graph)
	putStrLn $ "Connected values: " ++ (show $ connectedValues graph)
	putStrLn $ "Start values: " ++ (show $ startValues graph)
	putStrLn $ "End values: "   ++ (show $ endValues graph)
--}	putStrLn $ "Walk max cost path: " ++ (show $ take 10 $ walk (getTo . (maximumBy (comparing getCost))) graph 'A')
	putStrLn $ "Paths from A to E: "  ++ (show $ findPaths 'A' 'E' graph)
	putStrLn $ "Minimum Spanning Tree (Kruskal): "  ++ (show $ minimumSpanning_kruskal (comparing getCost) graph)
	putStrLn $ "Path 1 -> 3 (Djikstra): "  ++ (show $ findPath_djikstra 'A' 'B' graph)
	where
		graph = 
			GraphEdge 'A' 'B' 6 :
			GraphEdge 'A' 'C' 3 :

			GraphEdge 'B' 'A' 6 :
			GraphEdge 'B' 'E' 8 :

			GraphEdge 'C' 'A' 3 :
			GraphEdge 'C' 'D' 5 :
			GraphEdge 'C' 'E' 5 :
			
			GraphEdge 'D' 'C' 5 :
			GraphEdge 'D' 'E' 3 :
			GraphEdge 'D' 'G' 10 :

			GraphEdge 'E' 'B' 8 :
			GraphEdge 'E' 'C' 5 :
			GraphEdge 'E' 'D' 3 :
			GraphEdge 'E' 'F' 5 :
			GraphEdge 'E' 'G' 4 :

			GraphEdge 'F' 'E' 5 :
			GraphEdge 'F' 'G' 2 :
			
			GraphEdge 'G' 'D' 10 :
			GraphEdge 'G' 'E' 4 :
			GraphEdge 'G' 'F' 2 :

			[]
