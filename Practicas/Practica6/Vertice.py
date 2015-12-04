from Arista import *

class Vertex:
	"""constructor de los vertices"""
	element =""
	listEdge = []
	
	def __init__(self, element):
		self.element = element

	def getElement(self):
		return self.element

	def ponArista(self, arista):
		self.listEdge.append(arista)
		
	def printVertice(self):
		print self.element

	def neighbours(self):
		return self.listEdge
		
	def degree(self):
		return len(self.listEdge)
