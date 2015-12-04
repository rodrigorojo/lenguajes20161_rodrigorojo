from Arista import *

class Vertex:
	"""constructor de los vertices"""
	element =""
	vecinos = []
	grado = 0
	
	def __init__(self, element):
		self.element = element

	def getElement(self):
		return self.element

	def incrementaGrado(self):
		self.grado = self.grado + 1 

	def agregaVecino(self, vecino):
		if self.vecinos.count(vecino) == 0:
			self.vecinos.append(vecino)
		
	def printVertice(self):
		print self.element, "grado: ", self.grado

	def neighbours(self):
		return vecinos
		
	def degree(self):
		return grado
