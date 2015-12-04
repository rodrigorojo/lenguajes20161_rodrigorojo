from Vertice import *

class Edges:
	peso = 0.0
	"""si la grafica es no dirigida se puede ir de origen a destino y de destino a origen son solo nombres"""
	origen = ""
	destino = ""
	def __init__(self,origen,destino,peso):
		self.origen = origen
		self.destino = destino
		self.peso = peso

	def printArista(self):
		print self.origen.getElement(), self.destino.getElement(), self.peso

	def svertex(self):
		return self.origen

	def tvertex(self):
		return self.destino

	def weight(self):
		return self.weight
