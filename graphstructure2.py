import struct

class GraphEdge:
	def __init__(self,_fromValue,_toValue,cost,directed):
		self._fromValue = _fromValue
		self._toValue = _toValue
		self.directed = directed
		self.cost = cost

	def __eq__(self,other):
		return ((isinstance(other,self.__class__) and self.__dict__ == other.__dict__) or (self._fromValue == other._fromValue and self._toValue == other._toValue) or (not self.directed and not other.directed and self._fromValue == other._toValue and self._toValue == other._fromValue))

	def __ne__(self, other):
		return not self.__eq__(other)

	def __hash__(self):
		return hash((self._fromValue,self._toValue))

	def __str__(self):
		return "(" + str(self._fromValue) + (" -" + ("-" if self.cost==0 else "("+str(self.cost)+")") + "> " if self.directed else " <" + ("-" if self.cost==0 else "("+str(self.cost)+")") + "> ") + str(self._toValue) + ")"

	def haveFromValue(self,value):
		""" Tests if the given value is a from value for the edge
		This is only required for compatibility with undirected graphs and is the same function as `haveToValue` for undirected graphs
		"""
		return self._fromValue == value or ((not self.directed) and self._toValue == value)

	def haveToValue(self,value):
		""" Tests if the given value is a to value for the edge
		This is only required for compatibility with undirected graphs and is the same function as `haveFromValue` for undirected graphs
		"""
		return self._toValue == value or ((not self.directed) and self._fromValue == value)

	def otherValue(self,value):
		""" Returns the other value aside from the given value if it is in the node
		This is only used and required for compatibility with undirected graphs
		"""
		if self._fromValue == value:
			return self._toValue
		elif self._toValue == value:
			return self._fromValue
		else:
			return Null

	def getFromValue(self,otherValue):
		return self._fromValue if self.directed else (self.otherValue(otherValue))

	def getToValue(self,otherValue):
		return self._toValue if self.directed else (self.otherValue(otherValue))

class GraphPath:
	def __init__(self,vertexPath=[]):
		self.vertexPath = vertexPath

	def __eq__(self,other):
		return ((isinstance(other,self.__class__) and self.__dict__ == other.__dict__) or self.vertexPath == other.vertexPath)

	def __ne__(self, other):
		return not self.__eq__(other)

	def __lt__(self, other):
		return isinstance(other,self.__class__) and self.vertexPath.length() < other.vertexPath.length()

	def __gt__(self, other):
		return isinstance(other,self.__class__) and self.vertexPath.length() > other.vertexPath.length()

	def __hash__(self):
		return hash(self.vertexPath)

	def __str__(self):
		return reduce((lambda out,vertex: out + " -(" + str(vertex[0]) + ")> " + str(vertex[1])),self.vertexPath,"") if self.vertexPath else ""

	def add(self,cost,vertexTo):
		self.vertexPath.append((cost,vertexTo))

	def vertices(self):
		return map(lambda vertex: vertex[1],self.vertexPath)

	def costs(self):
		return map(lambda vertex: vertex[0],self.vertexPath)

class EdgeGraph:
	def __init__(self):
		""" Constructor """
		self.edges = set()

	def __str__(self):
		""" Prints the edges """
		return reduce((lambda out,edge: out + str(edge) + ", "),self.edges,"") if self.edges else ""

	def addEdge(self,nodeFrom,nodeTo,cost=0,directed=True):
		""" Adds an edge from the graph (Connects two values)

		Args:
			nodeFrom: The value to connect from
			nodeTo  : The value the connection points to

		"""
		self.edges.add(GraphEdge(nodeFrom,nodeTo,cost,directed))
		return self

	def removeEdge(self,nodeFrom,nodeTo):
		""" Removes an edge from the graph (Disconnects two values)

		Args:
			nodeFrom: The start value the connection
			nodeTo  : The end value the connection
		"""
		self.edges.remove(node)
		return self

	def findPath(self,fromValue,toValue,path=[]):#TODO: Path Iterable. 
		""" Finds a path from one vertex to another, returning the path using the vertices
		The code comments will refer to the parameter
		`fromValue` using the phrase "current vertex" and
		`toValue`   using the phrase "end vertex"     """
		# Return an empty list if the current vertex can be found in the path. This is for preventing an infinite loop when the following pattern exists: a->b, b->a
		if fromValue in path:
			return []

		# Appends the current vertex to the current path because this can be a possible path to the end vertex
		path.append(fromValue)

		# For each edge, look for the current vertex in the edges to find a path
		for edge in self.edges:
			# If the current vertex is found in an edge
			if edge.haveFromValue(fromValue):
				# If the end vertex is found
				if edge.haveToValue(toValue):
					# Append the last vertex to our path and return it
					path.append(toValue)
					return path
				else:
					# This may be a possible path to the end vertex, search for a further path in the found edge
					testPath = self.findPath(edge.getToValue(fromValue),toValue,list(path))

					# Also preventing infinite loop as it says above
					if testPath:
						return testPath
		return []

	def findPaths(self,fromValue,toValue,postPath=[],toValues=[]):
		""" Finds all paths from one vertex to another, returning the paths using the vertices
		The code comments will refer to the parameter
		`fromValue` using the phrase "current vertex" and
		`toValue`   using the phrase "end vertex"         """
		# This implementation is seaching from the end of the path to the beginning

		# The toValues list is required to prevent infinite loops while searching for paths.
		# Deep copy the list of already checked toValues for all paths. Not nessecary if only `unique` paths are needed
		toValues=list(toValues)
		toValues.append(toValue)
		
		# Initialize the paths list
		paths = []

		#For each edge, searching for matching toValue
		for edge in self.edges:
			if edge.haveToValue(toValue):
				# Preventing infinite loop by checking if the value already has been checked
				if edge.getFromValue(toValue) in toValues:
					continue

				# If the beginning of the path is found
				if edge.haveFromValue(fromValue):
					# Assemble a complete path and add it to the list
					newPath = [fromValue]
					newPath.append(toValue)
					newPath.extend(postPath)
					paths.append(newPath)
				else:
					# Continue through vertices looking for the beginning
					newPostPath = list(postPath)
					newPostPath.insert(0,toValue)
					paths.extend(self.findPaths(fromValue,edge.getFromValue(toValue),newPostPath,toValues))
		return paths

	
#	def findPaths2(self,fromValue,toValue,paths=[[]]):
#	""" Finds all patha from one vertex to another, returning the patha using the vertices
#		The code comments will refer to the parameter
#		`fromValue` using the phrase "current vertex" and
#		`toValue`   using the phrase "end vertex"     """
#		# Prevent infinite loop by not including duplicates of one vertex
#		if fromValue in paths[-1]:
#			paths[-1]=[]
#			return paths
#
#		# Appends the current vertex to the current path because this can be a possible path to the end vertex
#		paths[-1].append(fromValue)
#
#		# For each edge, look for the current vertex in the edges to find a path
#		found = False
#		for edge in self.edges:
#			# If the current vertex is found in an edge
#			if fromValue == edge.fromValue:
#				# If the end vertex is found
#				if toValue == edge.toValue:
#					# Append the last vertex to the current path and report that this path found an end vertex
#					paths.insert(-1,list(paths[-1]))
#					paths[-2].append(toValue)
#					found = True
#				else:
#					# There may be a path to the end vertex, search for further paths in the found edge
#					testPaths = self.findPaths(edge.toValue,toValue,[list(paths[-1])])
#
#					# If a path were found
#					if testPaths[-1]:
#					#	tmp = paths.pop()
#						paths.extend(testPaths)
#					#	paths.append(tmp)
#		
#		# If nothing was found, this path can be trashed
#		#if not found:
#		#	paths[-1]=[]
#
#		return paths

	def findPath_breadthFirst(self,fromValue,toValue):
		def f(fromValues):
			

		fromValues = []

		# Find initial nodes
		for edge in self.edges:
			if edge._fromValue == fromValue:
				fromValues.append(edge._toValue)
		return f(fromValues)



	def size(self):
		""" Returns the size of the graph. The number of edges. """
		return len(self.edges)

	def order(self):
		""" Returns the order of the graph. The number of vertices. """
		return len(self.vertices())

	def exportFile(self,filename):
		""" Exports the graph structure to a file """
		# Open file for writing
		f = open(filename,'w')
		# For each edge
		for edge in self.edges:
			# Write the edge data
			f.write(struct.pack('!2I',edge._fromValue,edge._toValue)) 
		# Close file
		f.close()

	def importFile(self,filename):
		""" Imports graph structure data from a file to the graph """
		# Open file for reading
		f = open(filename,'r')
		# Read the whole file to fileContent
		fileContent=f.read() 
		# Close file
		f.close()

		# While the fileContent string is holding data
		unpackSize=struct.calcsize('!2I') # Calculate the read length
		while fileContent:
			# Read data from fileContent
			edgeTuple=struct.unpack('!2I',fileContent[:unpackSize])
			# Add the edge from the data
			self.addEdge(edgeTuple[0],edgeTuple[1])
			# Remove the data that has been read from the fileContent string
			fileContent=fileContent[unpackSize:]

	def vertices(self):
		""" Returns the vertices as a set collection """
		# Use a set for collecting edges
		v = set()
		# For each edge
		for edge in self.edges:
			# Add the vertices stored in the edges. There will be no duplicates because of the set structure
			v.add(edge._fromValue)
			v.add(edge._toValue)
		# Return collected vertices
		return v

	def edgesOf(self,value):
		""" Returns the edges of a value """
		return map((lambda link: link.getToValue(value)),filter((lambda link: link.haveFromValue(value)),self.edges))

#graph.importFile("graphstructure2.dat")
graph = EdgeGraph()
#graph.addEdge(1,2).addEdge(1,3).addEdge(1,4).addEdge(1,5)
#graph.addEdge(2,1).addEdge(2,3).addEdge(2,4).addEdge(2,5)
#graph.addEdge(3,1).addEdge(3,2).addEdge(3,4).addEdge(3,5)
#graph.addEdge(4,1).addEdge(4,2).addEdge(4,3).addEdge(4,5)
#graph.addEdge(5,1).addEdge(5,2).addEdge(5,3).addEdge(5,4)
graph.addEdge(1,2,30,False).addEdge(2,3,50,False).addEdge(3,1,80,False)
#graph.addEdge(1,2).addEdge(1,3).addEdge(3,2,0,False).addEdge(3,4)
#graph.addEdge(1,2).addEdge(1,3).addEdge(3,2,0,False).addEdge(3,4)
#graph.exportFile("graphstructure2.dat")

print "Edges:",graph
print "Path:",graph.findPath(1,3)
print "Paths:",graph.findPaths(1,3)
print "Size:",graph.size()
print "Order:",graph.order()
print "Vertices:",graph.vertices()
print "Edges of 1:",graph.edgesOf(1)
print "Edges of 2:",graph.edgesOf(2)
print "Edges of 3:",graph.edgesOf(3)
print "Edges of 4:",graph.edgesOf(4)
print "Edges of 5:",graph.edgesOf(5)

path = GraphPath()
path.add(40,1)
path.add(30,2)
path.add(20,3)
print path
print path.vertices()
print path.costs()," => ",sum(path.costs())
