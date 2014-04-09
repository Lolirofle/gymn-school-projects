module Graph where

class Graph g where
	-- |Empty graph
	empty :: g a

	isEmpty :: g a -> Bool
	isEmpty graph = null (edges graph) && null (vertices graph)

	-- |Edges in the graph
	edges :: g a -> [(a,a)]

	-- |Vertices in the graph
	vertices :: g a -> [a]

	-- |Size of graph (Number of edges/connections)
	size :: g a -> Int
	size = length . edges

	-- |Order of graph (Number of vertices/nodes)
	order :: g a -> Int
	order = length . vertices

	toLabelsFrom :: Eq a => a -> g a -> [a]

	hasEdge :: Eq a => a -> a -> g a -> Bool
	hasEdge from to graph = elem (from,to) (edges graph)

	-- |Graph with specific vertex added, if not already existing
	withVertex :: Eq a => a -> g a -> g a

	-- |Graph with specific vertex removed, if existing
	withoutVertex :: Eq a => a -> g a -> g a

	-- |Graph with specific edge added, if  not already existing
	withEdge :: Eq a => a -> a -> g a -> g a

	-- |Graph with specific edge removed, if existing
	withoutEdge :: Eq a => a -> a -> g a -> g a

-- |The path it walks when the `routeDecider` is applied at every intersection, deciding based on a criteria from the current vertex and the graph
walk :: (Graph g,Eq a) => (a -> g a -> Maybe a) -> a -> g a -> [a]
walk routeDecider start graph = case nextRoute of
		Just route -> start:(walk (routeDecider) route graph)
		Nothing    -> [start]
	where
		nextRoute = routeDecider start graph

-- |The path it walks when the `routeDecider` is applied at every intersection, deciding between a list of vertices
walk' :: (Graph g,Eq a) => ([a] -> Maybe a) -> a -> g a -> [a]
walk' routeDecider start graph = walk (routeDecider') start graph
	where
		routeDecider' currentRoute graph =  
			if null routes then
				Nothing
			else
				nextRoute
			where
				routes    = Graph.toLabelsFrom currentRoute graph
				nextRoute = routeDecider routes

class Vertex v where
	vertexIsEqual :: v -> v -> Bool

data LabeledVertex label = LabeledVertex label

class Edge e where
	edgeRepresents :: e -> a -> a -> Bool

class Edge e => DirectedEdge e
class Edge e => UndirectedEdge e
class Edge e => WeightedEdge e

data LabeledEdge label = LabeledEdge label
