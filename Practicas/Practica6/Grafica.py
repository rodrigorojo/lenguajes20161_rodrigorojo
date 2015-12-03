from Vertice import *
from Arista import *

class Graph:
	"""Variables de clase: 
		Una lista de Vertices
		El numero de Aristas
		un booleano a manera de String que nos dira si la grafica es o no dirigida"""
	listaVertices = []
	bool_dirigida = ""
	listaAristas = []
	
	"""Constructor de la clase"""
	def __init__(self, listVertex,bool_direct, listEdges):
		self.listaVertices = listVertex
		self.bool_dirigida = bool_direct
		self.listaAristas = listEdges
	
	def vertices(self):
		return self.listaVertices

	def edges(self):
		return self.listaAristas
	
	def directed(self):
		return self.bool_dirigida
