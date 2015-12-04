from Vertice import *
from Arista import *

class Graph:
	"""Variables de clase: 
		Una lista de Vertices
		El numero de Aristas
		un booleano a manera de String que nos dira si la grafica es o no dirigida"""
	listaVertices = []
	listaAux = []
	bool_dirigida = ""
	listaAristas = []
	
	"""Constructor de la clase"""
	def __init__(self, listVertex,bool_direct, listEdges):
		self.listaVertices = listVertex
		self.bool_dirigida = bool_direct
		self.listaAristas = listEdges

	def setDirected(self, directed):
		self.bool_dirigida = directed

	def agregaVertice(self, elemento):
		v = Vertex(elemento)
		self.listaAux.append(elemento)
		self.listaVertices.append(v)
	
	def agregaArista(self, origen, destino, peso):
		i = self.listaAux.index(origen)
		j = self.listaAux.index(destino)
		a = Edges(self.listaVertices[i], self.listaVertices[j], peso)
		self.listaAristas.append(a)
		self.listaVertices[i].incrementaGrado()
		self.listaVertices[i].agregaVecino(destino)
		self.listaVertices[j].incrementaGrado()
		self.listaVertices[j].agregaVecino(origen)

	def vertices(self):
		return self.listaVertices

	def edges(self):
		return self.listaAristas
	
	def directed(self):
		return self.bool_dirigida
