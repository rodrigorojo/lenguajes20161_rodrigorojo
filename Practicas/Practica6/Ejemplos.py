from Grafica import *
from Graphreader import *
from Vertice import *
from Arista import *

graphreaderjson = GraphReader('graph.json')
graficajson = graphreaderjson.readJSON()

graficacsvreader = GraphReader('graph.csv')
graficacsv = graficacsvreader.readCSV()

graphreaderxml = GraphReader('graph.xml')
graficaxml = graphreaderxml.readXML()

print "prueba: CSV"
print "vertices:"
lv = graficacsv.vertices()
for v in lv:
	v.printVertice()
print "aristas:"
la = graficacsv.edges()
for a in la:
	a.printArista()
print "directed: "
b = graficacsv.directed()
print b


print "prueba: JSON "
print "vertices: "
lv2 = graficajson.vertices()
for v in lv2:
	v.printVertice()
print "aristas: "
la2 = graficajson.edges()
for a in la2:
	a.printArista()
print "directed: "
b2 = graficajson.directed()
print b2

print "prueba: XML "
print "vertices: "
lv3 = graficaxml.vertices()
for v in lv3:
	v.printVertice()
print "aristas: "
la3 = graficaxml.edges()
for a in la3:
	a.printArista()
print "directed: "
b3 = graficaxml.directed()
print b3


