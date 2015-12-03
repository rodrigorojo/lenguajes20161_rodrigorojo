import csv
import xml.etree.ElementTree as ET
import json
from pprint import pprint

class Graph:
	"""Variables de clase: 
		Una lista de Vertices
		El numero de Aristas
		un booleano a manera de String que nos dira si la grafica es o no dirigida"""
	listaVertices = []
	numArist = 0
	bool_dirigida = ""
	listaAristas = []
	
	"""Constructor de la clase"""
	def __init__(self, listVertex, edgeNumber,bool_direct, listEdges):
		self.listaVertices = listVertex
		self.numArist = edgeNumber
		self.bool_dirigida = bool_direct
		self.listaAristas = listEdges
	
	
	def vertices(self):
		l = []
		for v in self.listaVertices:
			l.append(v)
		print l
		
	def edges(self):
		l =[]
		for e in self.listaAristas:
			l.append(e)
		print l
	
	
	
	def directed(self, identify):
		if identify == 0:
			return False
		elif identify == 1:
			return True
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
	
	def __init__(self, element):
		self.element = element
		
	
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
vertex = Vertex("")
graph = Graph([vertex],0,"",[])

class GraphReader():
	
	def __init__(self,ruta):
		self.ruta = ruta
	
	def getGraph(self):
		return self.graph
	
	def readJSON(self):
		with open(self.ruta, 'rb') as data_file:
			data = json.load(data_file)
			dirigida = data["direct"]
			graph.bool_dirigida = dirigida
			list_vertex =[]
			list_edges = []
			for vertex in data["vertices"]:
				nombre = str(vertex[0])
				list_vertex.append(nombre)
			graph.listaVertices = list_vertex				
			for edge in data["edges"]:
				v1 = str(edge[0])
				v2 = str(edge[1])
				p = str(edge[2])
				list_edges.append([v1, v2, p])
			graph.numArist = len(list_edges	)
			graph.listaAristas = list_edges
	def readCSV(self):
		with open(self.ruta, 'rb') as f:
		    reader = csv.reader(f, delimiter=',', quoting=csv.QUOTE_NONE)
<<<<<<< HEAD
=======
		    lista = []
>>>>>>> c953c21fab0c8c406f2f9dd27cfbbf489ae5cd4a
		    for row in reader:
		        lista.append(row)
		return lista
	def readXML(self):
		tree = ET.parse(self.ruta)
		root = tree.getroot()
		for child in root:
		   print child.tag, child.attrib


graphreader = GraphReader('graph.json')
graphreader.readJSON()

graphreader = GraphReader('graph.csv')
"""print graphreader.readCSV()"""

graphreader = GraphReader('graph.xml')
"""print graphreader.readXML()"""



print "prueba: JSON "
print "Aristas: "
print graph.edges()
print "Pesos : "
pass
print "Vertices: "
print graph.vertices()
print "directed: "
print graph.directed(graph.bool_dirigida)

 


