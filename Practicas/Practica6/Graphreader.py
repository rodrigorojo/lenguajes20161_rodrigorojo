from Grafica import *
from Vertice import *
from Arista import *
import csv
import xml.etree.ElementTree as ET
import json
from pprint import pprint

class GraphReader():
	
	def __init__(self,ruta):
		self.ruta = ruta
	
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
				a = Edges(v1,v2,p)
				lista_aristas.append(a)
		g = Graph(lista_vertices, i, lista_aristas)
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
		        	a = Edges(e1,e2,e3)
		        	lista_aristas.append(a)
		g = Graph(lista_vertices, i, lista_aristas)
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
		    	a = Edges(n[12],n[27],n[42])
		    	list_edges.append(a)
		g = Graph(list_vertex, i, list_edges)
		return g
				
		
