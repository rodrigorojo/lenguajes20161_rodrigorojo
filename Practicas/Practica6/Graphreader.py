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
		i = ""
		g = Graph([], "", [])
		with open(self.ruta, 'rb') as data_file:
			data = json.load(data_file)
			x = data["direct"]
			if x == 1:
				i = True
			else:
				i = False
			for vertex in data["vertices"]:
				g.agregaVertice(vertex[0])				
			for edge in data["edges"]:
				v1 = str(edge[0])
				v2 = str(edge[1])
				p = str(edge[2])
				g.agregaArista(v1,v2,p)
		g.setDirected(i)
		return g

	def readCSV(self):
		lista_aux =[]
		i = ""
		g = Graph([], "", [])
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
		        		g.agregaVertice(e1)
		        	if lista_aux.count(e2) == 0:
		        		lista_aux.append(e2)
		        		g.agregaVertice(e2)
		        	e3 = row[2].replace(' ','')
		        	g.agregaArista(e1,e2,e3)
		g.setDirected(i)
		return g

	def readXML(self):
		tree = ET.parse(self.ruta)
		root = tree.getroot()
		i = ""
		g = Graph([], "", [])
		d = str(root.attrib)
		if root.attrib.values()[0] == '1':
		    i = True
		else:
		    i = False
		for child in root:
		    n = str(child.attrib.values()[0])
		    if child.tag == 'vertex':
		    	g.agregaVertice(n)
		    else:
		    	g.agregaArista(child.attrib.values()[0],child.attrib.values()[1],
		    								child.attrib.values()[2])
		g.setDirected(i)
		return g
				
		
