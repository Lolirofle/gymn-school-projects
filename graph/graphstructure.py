class DirectedGraphNode:
	def __init__(self,value):
		self.value = value
		self.links = set()

	def __str__(self):
		return str(self.value) + (" -> (" + reduce((lambda out,link: out + str(link) + ", "),self.links,"") + ")" if self.links else "")

	def __eq__(self,other):
		return (isinstance(other,self.__class__) and self.__dict__ == other.__dict__)

	def __ne__(self, other):
		return not self.__eq__(other)

	def __hash__(self):
		return hash(self.value)

	def addNode(self,node):
		self.links.add(node)

	def removeNode(self,node):
		self.links.remove(node)


graph = DirectedGraphNode(5)
graph.addNode(DirectedGraphNode(6))
graph.addNode(DirectedGraphNode(7))

print graph
