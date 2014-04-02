module EdgeGraph where

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

import Control.Monad
import Data.Functor
import Control.Applicative

-- |Graph Edge structure. Represents the edges of a graph
data GraphEdge a cost = GraphEdge a a cost

getFrom (GraphEdge from _ _) = from 
getTo   (GraphEdge _ to _)   = to
getCost (GraphEdge _ _ cost) = cost

getFroms :: EdgeGraph a b -> [a]
getFroms = map getFrom

getTos   :: EdgeGraph a b -> [a]
getTos   = map getTo

getCosts :: EdgeGraph a b -> [b]
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

type EdgeGraph node cost = [GraphEdge node cost]

-- |Size of graph (Number of edges)
size :: EdgeGraph a b -> Int
size = length

-- |Edges in the graph
edges :: EdgeGraph a b -> EdgeGraph a b
edges = id

-- |Vertices in the graph
vertices :: Eq a => EdgeGraph a b -> [a]
vertices edges = foldl buildUnique [] edges where
	buildUnique result (GraphEdge from to _) = 
		(if (elem from result) then result2 else from:result2) where result2 = (if (elem to result) then result else to:result)

-- |Order of graph (Number of vertices)
order :: Eq a => EdgeGraph a b -> Int
order = length . vertices

-- |Graph with specific edge added
addEdge :: Eq a => GraphEdge a b -> EdgeGraph a b -> EdgeGraph a b
addEdge edge edges = if (elem edge edges) then edges else edge:edges

-- |Graph with specific edge removed
removeEdge :: Eq a => GraphEdge a b -> EdgeGraph a b -> EdgeGraph a b
removeEdge = delete

-- |The edges of a label
edgesOf :: Eq a => a -> EdgeGraph a b -> [GraphEdge a b]
edgesOf label edges = (filter (\(GraphEdge from to _) -> from == label || to == label) edges)

-- |The to labels of a from label
toEdgesFrom :: Eq a => a -> EdgeGraph a b -> [GraphEdge a b]
toEdgesFrom label edges = filter (\(GraphEdge from _ _) -> from == label) edges

-- |The from labels of a to label
fromEdgesTo :: Eq a => a -> EdgeGraph a b -> [GraphEdge a b]
fromEdgesTo label edges = filter (\(GraphEdge _ to _) -> to == label) edges

-- |The to labels of a from label
toLabelsFrom :: Eq a => a -> EdgeGraph a b -> [a]
toLabelsFrom label edges = map getTo (toEdgesFrom label edges)

-- |The from labels of a to label
fromLabelsTo :: Eq a => a -> EdgeGraph a b -> [a]
fromLabelsTo label edges = map getFrom (fromEdgesTo label edges)

-- |The path it walks when the `routeChooser` is applied at every intersection
walk :: Eq a => (EdgeGraph a b -> a) -> EdgeGraph a b -> a -> [a]
walk routeChooser graph start = if null routes then [start] else start:(walk routeChooser graph (routeChooser routes)) where
	routes = toEdgesFrom start graph

-- |The path it walks backwards when the `routeChooser` is applied at every intersection
walkBackwards :: Eq a => (EdgeGraph a b -> a) -> EdgeGraph a b -> a -> [a]
walkBackwards routeChooser graph start = if null routes then [start] else start:(walk routeChooser graph (routeChooser routes)) where
	routes = fromEdgesTo start graph


-- |The start vertices. No labels point to these (High complexity because of nub and the elem in the filter?)
startVertices :: Eq a => EdgeGraph a b -> [a]
startVertices graph = filter (`notElem` (getTos graph)) (nub $ (getFroms graph))

-- |The end vertices. No labels start these (High complexity because of nub and the elem in the filter?)
endVertices :: Eq a => EdgeGraph a b -> [a]
endVertices graph = filter (`notElem` (getFroms graph)) (nub $ (getTos graph))

-- |The connected vertices that is neither a start or end vertex.
internalVertices :: Eq a => EdgeGraph a b -> [a]
internalVertices graph = nub (uncurry intersect (unzip (map (\edge -> (getFrom edge,getTo edge)) graph)))

isInternalVertex :: Eq a => a -> EdgeGraph a b -> Bool
isInternalVertex vertex graph = isJust $ find (\edge -> (getFrom edge)/=vertex && (getTo edge)/=vertex) graph

getEdge :: Eq a => a -> a -> EdgeGraph a b -> Maybe (GraphEdge a b)
getEdge from to graph = find (\edge -> from == getFrom edge && to == getTo edge) graph

--TODO: This should behave differently if an undirected graph is implemented
isAdjacent :: Eq a => a -> a -> EdgeGraph a b -> Bool
isAdjacent a b graph = isJust (getEdge a b graph)

-- |Simplifies a graph by summing the costs and having as few intersections and paths as possible
--simplify :: (a -> a -> Maybe a) -> (b -> b -> b) -> EdgeGraph a b -> EdgeGraph a b
--simplify vertexSumFunc costSumFunc graph = 

-- |Finds all paths from one label to another label
findPaths :: Eq a => a -> a -> EdgeGraph a b -> [[a]]
findPaths from to graph = findPaths' from to graph [] where
	findPaths' from to graph path = foldl f [] (fromLabelsTo to graph) where
		f result fromValue =
			-- If already checked the `fromValue` before in the path, skip and avoid infinite loop
			if elem fromValue path then
				result
			-- If the searched from label is found, then it is at the beginning and shoorduld construct the list entry of the path
			else
				let newPath = to : path in
				if fromValue==from then
					(from : newPath) : result
				-- Else search for new paths to the beginning from the current `fromValue`
				else
					result ++ (findPaths' from fromValue graph (newPath))

-- |Lists all vertices that are connected to v, all of the vertices on its tree
verticesInTreeOf :: Eq a => a -> EdgeGraph a b -> [a]
verticesInTreeOf v graph = foldl f [v] (fromLabelsTo v graph) where
		f result fromValue =
			-- If already checked the `fromValue` before
			if elem fromValue result then
				result
			else
				foldl f (fromValue:result) (fromLabelsTo fromValue graph)

-- Simple implementation of Kruskals minimum spanning tree algorithm. It may be possible to optimize if it can anknowledge that the graph already is spanned in the middle of the fold
minimumSpanning_kruskal :: (Eq a) => (GraphEdge a b -> GraphEdge a b -> Ordering) -> EdgeGraph a b -> EdgeGraph a b
minimumSpanning_kruskal compareFunc graph = foldl buildMstAvoidingCycles [] sorted where
	-- The cost sorted list
	sorted = sortBy compareFunc graph
	buildMstAvoidingCycles result edge = 
		if elem (getFrom edge) (verticesInTreeOf (getTo edge) result) then
			result
		else
			edge : result

-- |The list of tuples returned that represents a "box" have the following fields:
-- |(vertex,pathCost,previousVertexInPath,permanent)
-- |   vertex              : The vertex the current box represents
-- |   pathCost            : The cost for getting from the specified vertex (`from`) to this vertex
-- |   previousVertexInPath: By following all the previous vertices, a path is created which also is the cheapest route
-- |   permanent           : Used internally in the function
data DjikstraGraphBox a cost = DjikstraGraphBox a cost (Maybe a) Bool deriving (Eq,Show)

djikstraPath_costTo :: Eq a => a -> [DjikstraGraphBox a b] -> Maybe b
djikstraPath_costTo to paths = case (find (\(DjikstraGraphBox vertex _ _ _) -> vertex==to) paths) of
	Just (DjikstraGraphBox _ cost _ _) -> Just cost
	Nothing -> Nothing

djikstraPath_from :: [DjikstraGraphBox a b] -> a
djikstraPath_from paths = case (find (\(DjikstraGraphBox _ _ prev _) -> isNothing prev) paths) of
	Just (DjikstraGraphBox vertex _ _ _) -> vertex
	Nothing -> error "djikstraPath_from: Cannot find the from vertex"

djikstraPath_pathTo :: Eq a => a -> [DjikstraGraphBox a b] -> [a]
djikstraPath_pathTo to paths = case (find (\(DjikstraGraphBox vertex _ _ _) -> vertex==to) paths) of
	Just (DjikstraGraphBox _ _ prev _) -> case prev of
		Just v  -> to : (djikstraPath_pathTo v paths)
		Nothing -> to : []
	Nothing -> error "djikstraPath_pathTo: Invalid path description. Cannot find a vertex from the previousVertexInPath field"

-- TODO: Not optimized. Doesn't work for negative numbers? (E.g. 1+2 = 3, sum is greater. -1+-2 = -3, sum is lesser)
findPath_djikstra :: (Eq a,Num b,Ord b) => a -> EdgeGraph a b -> [DjikstraGraphBox a b]
findPath_djikstra from graph = updateBoxed [DjikstraGraphBox from 0 Nothing True] where
	lengthAtLeast :: Int -> [a] -> Bool
	lengthAtLeast 0 _        = True
	lengthAtLeast _ []       = False
	lengthAtLeast n (_:tail) = lengthAtLeast (n-1) tail

	updateBoxed initialBoxed =
		-- While there's still non-permanent boxes and it isn't the first loop
		if (isNothing $ find (\(DjikstraGraphBox _ _ previousVertexInPath permanent) -> not permanent) initialBoxed) && (lengthAtLeast 2 initialBoxed) then
			initialBoxed
		else
			updateBoxed (permanentBoxCheapest $ foldl selectBoxed initialBoxed (toEdgesFrom (vertexOfBox $ head initialBoxed) graph)) where
				-- The boxed labels that are non permanent
				nonPermanentBoxed boxed = (filter (\(DjikstraGraphBox _ _ _ permanent) -> not permanent) boxed)

				-- Extracts the vertex from a box
				vertexOfBox (DjikstraGraphBox vertex _ _ _) = vertex

				-- Extracts the path cost from a box
				pathCostOfBox (DjikstraGraphBox _ pathCost _ _) = pathCost

				-- Replaces the cheapest box to a permanent version of it
				permanentBoxCheapest boxed = (permanentBox cheapest) : (delete cheapest boxed) where
					-- The box that has the cheapest path cost
					cheapest = minimumBy (comparing pathCostOfBox) (nonPermanentBoxed boxed)

					-- Returns a permanent version of the given box
					permanentBox (DjikstraGraphBox vertex pathCost previousVertexInPath _) = (DjikstraGraphBox vertex pathCost previousVertexInPath True)

				selectBoxed boxed (GraphEdge from to cost) =
					case (find (\(DjikstraGraphBox vertex _ _ _) -> vertex == from) boxed) of
						Just (DjikstraGraphBox previousVertex previousPathCost _ _) -> 
							-- Look for a box with the same vertex
							let newBox = (DjikstraGraphBox to (cost+previousPathCost) (Just previousVertex) False) in
							case (find (\(DjikstraGraphBox vertex _ _ _) -> vertex == to) boxed) of
								-- If found and it's permanent or the new cost is higher than the old, keep boxed as it is
								Just box@(DjikstraGraphBox _ pathCost _ permanent) | (pathCostOfBox newBox)>pathCost || permanent -> boxed
								-- If it doesn't hold for the criteria above but still exists, modify (remove, construct and insert) the box
								Just box -> newBox : (delete box boxed)
								-- If it's a vertex that hasn't been boxed yet, insert it
								Nothing  -> newBox : boxed
						Nothing -> boxed

exportData :: (a -> Binary.Word32) -> EdgeGraph a b -> Binary.Put
exportData labelToWord graph = mapM_ serializeEdge graph where
	-- Function that serializes a single edge
	serializeEdge edge = do
		Binary.Put.putWord32be (labelToWord (getFrom edge))
		Binary.Put.putWord32be (labelToWord (getTo edge))

importData :: (Binary.Word32 -> a) -> Binary.Get (EdgeGraph a Int)
importData wordToValue = do
	-- If EOF
	empty <- Binary.Get.isEmpty
	if empty then 
		-- Return an empty list encapsulated in the Binary.Get monad
		return []
	else do
		-- Return a list of edges, recursively reading all the labels from the file
		edge <- (fmap (GraphEdge) (liftM wordToValue Binary.Get.getWord32be) <*> (liftM wordToValue Binary.Get.getWord32be) <*> (pure 0))
		edges <- importData wordToValue
		return (edge:edges)

main :: IO ()
main = do
	-- Export file
{--	file <- openBinaryFile "djikstra_example_graph2.dat" WriteMode
	ByteString.Lazy.hPut file (Binary.Put.runPut $ exportData (fromIntegral . ord) graph)
	hClose file
--}
{--	-- Import file
	file <- openBinaryFile "djikstra_example_graph.dat" ReadMode
	rawData <- ByteString.Lazy.hGetContents file
	let importedGraph = Binary.Get.runGet (importData (chr . fromIntegral)) rawData
	print importedGraph
	hClose file
--}

	putStrLn $ "Edges: "      ++ (show $ edges graph)
{--	putStrLn $ "Size: "       ++ (show $ size graph)
	putStrLn $ "Order: "      ++ (show $ order graph)
	putStrLn $ "Vertices: "   ++ (show $ vertices graph)
	putStrLn $ "Edges of 5: " ++ (show $ edgesOf 5 graph)
	putStrLn $ "To labels from 1: " ++ (show $ toLabelsFrom 1 graph)
	putStrLn $ "Connected labels: " ++ (show $ connectedLabels graph)
	putStrLn $ "Start labels: " ++ (show $ startLabels graph)
	putStrLn $ "End labels: "   ++ (show $ endLabels graph)
--}	--putStrLn $ "Walk max cost path: " ++ (show $ take 10 $ walk (getTo . (maximumBy (comparing getCost))) graph 'A')
	--putStrLn $ "Paths from A to E: "  ++ (show $ findPaths 'A' 'E' graph)
	putStrLn $ "Minimum Spanning Tree (Kruskal): "  ++ (show $ minimumSpanning_kruskal (comparing getCost) graph)
	
	let djikstraPaths = findPath_djikstra 'A' graph in do
		putStrLn $ "Djikstra: Paths from A: " ++ (show $ djikstraPaths)
		putStrLn $ "Djikstra: From: " ++ (show $ djikstraPath_from djikstraPaths)
		putStrLn $ "Djikstra: Cost to D: " ++ (show $ djikstraPath_costTo 'D' djikstraPaths)
		putStrLn $ "Djikstra: Cost to O: " ++ (show $ djikstraPath_costTo 'O' djikstraPaths)
		putStrLn $ "Djikstra: Path to D: " ++ (show $ djikstraPath_pathTo 'D' djikstraPaths)

		
	--putStrLn $ "Vertices in tree of D: " ++ (show $ take 10 (verticesInTreeOf 'F' graph))
	where
		graph = 
			GraphEdge 'A' 'B' 4 :
			GraphEdge 'A' 'G' 8 :
			GraphEdge 'A' 'F' 10 :

			GraphEdge 'B' 'A' 4 :
			GraphEdge 'B' 'G' 2 :
			GraphEdge 'B' 'C' 3 :

			GraphEdge 'C' 'B' 3 :
			GraphEdge 'C' 'G' 8 :
			GraphEdge 'C' 'D' 13 :

			GraphEdge 'D' 'C' 8 :
			GraphEdge 'D' 'G' 15 :
			GraphEdge 'D' 'E' 2 :

			GraphEdge 'E' 'F' 6 :
			GraphEdge 'E' 'G' 11 :
			GraphEdge 'E' 'D' 2 :

			GraphEdge 'F' 'A' 10 :
			GraphEdge 'F' 'G' 4 :
			GraphEdge 'F' 'E' 6 :
			
			GraphEdge 'G' 'A' 8 :
			GraphEdge 'G' 'B' 2 :
			GraphEdge 'G' 'C' 8 :
			GraphEdge 'G' 'D' 15 :
			GraphEdge 'G' 'E' 11 :
			GraphEdge 'G' 'F' 4 :

			[]
