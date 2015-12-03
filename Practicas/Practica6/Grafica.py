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
	
	
	def getVertices(self):
		return self.listaVertices
	def getAristas(self):
		return self.listaAristas

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
	
	def isDirected(self):
		return self.bool_dirigida
	
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
		
	def printVertice(self):
		print self.element
	def neighbours(self):
		return self.listEdge
	def degree(self):
		return len(self.listEdge)

	
		
class Arista:
	peso = 0.0
	"""si la grafica es no dirigida se puede ir de origen a destino y de destino a origen son solo nombres"""
	origen = ""
	destino = ""
	def __init__(self,origen,destino,peso):
		self.origen = origen
		self.destino = destino
		self.peso = peso
	def constructorAristas(origen,destino, peso):
		self.origen = origen
		self.destino = destino
		self.peso = peso
	def printArista(self):
		print self.origen, self.destino, self.peso
	def svertex(self):
		return self.origen
	def tvertex(self):
		return self.destino
	def weight(self):
		return self.weight
vertex = Vertex("")
graph = Graph([vertex],0,"",[])
graph1 = Graph([vertex],0,"",[])

class GraphReader():
	
	def __init__(self,ruta):
		self.ruta = ruta
	
	def getGraph(self):
		return self.graph
	
	def readJSON(self):
		lista_vertices = []
		lista_aristas = []
		i = ""
		with open(self.ruta, 'rb') as data_file:
			data = json.load(data_file)
			x = data["direct"]
			if x == 1:
				i = True
			else:
				i = False
			for vertex in data["vertices"]:
				v = Vertex(vertex[0])
				lista_vertices.append(v)				
			for edge in data["edges"]:
				v1 = str(edge[0])
				v2 = str(edge[1])
				p = str(edge[2])
				a = Arista(v1,v2,p)
				lista_aristas.append(a)
		g = Graph(lista_vertices, len(lista_aristas), i, lista_aristas)
		return g
	def readCSV(self):
		lista_vertices = []
		lista_aristas  = []
		lista_aux =[]
		i = ""
		with open(self.ruta, 'rb') as f:
		    reader = csv.reader(f, delimiter=',', quoting=csv.QUOTE_NONE)
		    for row in reader:
		        if row[0] == "direct=1":
		        	i = True
		        elif row[0] == "direct=0":
		        	i = False
		        else:
		        	e1 = row[0].replace('"', '')
		        	e2 = row[1].replace('"', '')
		        	e2 = e2.replace(' ', '')
		        	if lista_aux.count(e1) == 0:
		        		lista_aux.append(e1)
		        		v1 = Vertex(e1)
		        		lista_vertices.append(v1)
		        	if lista_aux.count(e2) == 0:
		        		lista_aux.append(e2)
		        		v2 = Vertex(e2)
		        		lista_vertices.append(v2)
		        	e3 = row[2].replace(' ','')
		        	a = Arista(e1,e2,e3)
		        	lista_aristas.append(a)
		g = Graph(lista_vertices, len(lista_aristas), i, lista_aristas)
		return g
	def readXML(self):
		tree = ET.parse(self.ruta)
		root = tree.getroot()
		list_vertex = []
		list_edges = []
		i = ""
		d = str(root.attrib)
		if d[12] == '1':
		    i = True
		else:
		    i = False
		for child in root:
		    n = str(child.attrib)
		    if child.tag == 'vertex':
		    	v = Vertex(n[11])
		    	list_vertex.append(v)
		    else:
		    	a = Arista(n[12],n[27],n[42])
		    	list_edges.append(a)
		g = Graph(list_vertex, len(list_edges), i, list_edges)
		return g
				
		

graphreaderjson = GraphReader('graph.json')
graficajson = graphreaderjson.readJSON()

graficacsvreader = GraphReader('graph.csv')
graficacsv = graficacsvreader.readCSV()

graphreaderxml = GraphReader('graph.xml')
graficaxml = graphreaderxml.readXML()

print "prueba: CSV"
print "vertices:"
lv = graficacsv.getVertices()
for v in lv:
	v.printVertice()
print "aristas:"
la = graficacsv.getAristas()
for a in la:
	a.printArista()
print "directed: "
b = graficacsv.isDirected()
print b


print "prueba: JSON "
print "vertices: "
lv2 = graficajson.getVertices()
for v in lv2:
	v.printVertice()
print "aristas: "
la2 = graficajson.getAristas()
for a in la2:
	a.printArista()
print "directed: "
b2 = graficajson.isDirected()
print b2

print "prueba: XML "
print "vertices: "
lv3 = graficaxml.getVertices()
for v in lv3:
	v.printVertice()
print "aristas: "
la3 = graficaxml.getAristas()
for a in la3:
	a.printArista()
print "directed: "
b3 = graficaxml.isDirected()
print b3


