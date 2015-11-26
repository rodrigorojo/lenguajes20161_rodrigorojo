class Graph:
	"""Variables de clase: 
		Una lista de Vertices
		El numero de Aristas
		un booleano a manera de String que nos dira si la grafica es o no dirigida"""
	listaVertices = []
	numArist = 0
	bool_dirigida = ""
	
	"""Constructor de la clase"""
	def __init__(self, listVertex, edgeNumber,bool_direct):
		self.listaVertices = listVertex
		self.numArist = edgeNumber
		self.bool_dirigida = bool_direct
	
	def returnVertex(self):
		l=[]
		for v in self.listaVertices:
			l.append(v.element)
		return l
	
	def isDirect(self):
		if self.bool_dirigida == "0":
			return True
		elif self.bool_dirigida == "1":
			return False
		else:
			return "Error, imposible graficar"
	
	"""Prototipo"""
	"""Regresamos las listas de vertices adyacentes de los vertices"""		
	def getEdge(self):
		for vertex in self.listaVertices:
			print vertex.listEdge
		
			

		
		
		
class Vertex:
	"""constructor de los vertices"""
	element =""
	distance = 0
	index = 0
	listEdge = []
	def __init__(self, element, distance, index, listEdge):
		self.element = element
		self.distance = distance
		self.index = index
		self.listEdge = listEdge
	
	def neighbours(self):
		return self.listEdge
	def degree(self):
		return len(self.listEdge)
		
		class Arista:
			peso = 0.0
			def __init__(self,adyacente,peso):
				self.adyacente = adyacente
				self.peso = peso
			def constructorAristas(origen,destino, peso):
				self.origen = origen
				self.destino = destino
				self.peso = peso
			def svertex(self):
				return self.origen
			def tvertex(self):
				return self.destino
			def weight(self):
				return self.weight

class GraphReader():
	vertex = Vertex("",0,0,[])
	graph = Graph([vertex],0,"")
	def __init__(self,ruta):
		self.ruta = ruta
	
	def getGraph(self):
		return self.graph
	
	def readJSON(self):
		list_lines = []
		input_file = open(self.ruta, 'rb')
		for line in input_file:
			list_lines.append(line)
		for line in list_lines:
			line.strip()
			
		return list_lines	

graphreader = GraphReader('graph.json')
print graphreader.readJSON()
			
vertex3 = Vertex(2,1,2,[1,2,4,3])
vertex2 = Vertex(3,3,4,[1,2,3])
vertex = Vertex("E",3,4,[1,2,3])
graph = Graph([vertex,vertex2,vertex3],2,"2")
print vertex3.degree()

