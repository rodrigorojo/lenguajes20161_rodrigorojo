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

print "prueba: ", graficacsvreader.ruta
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

print "\n"

print "prueba: ", graphreaderjson.ruta
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

print "\n"

print "prueba: ", graphreaderxml.ruta
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

print "\n"
"""
readerpetersencsv = GraphReader('petersen.csv')
petersencsv = readerpetersencsv.readCSV()
"""
readerpetersenjson = GraphReader('petersen.json')
petersenjson = readerpetersenjson.readJSON()

readerpetersenxml = GraphReader('petersen.xml')
petersenxml = readerpetersenxml.readXML()

"""
print "prueba: ", readerpetersencsv.ruta
print "Vertices: "
lv4= petersencsv.vertices()
for v in lv4:
	v.printVertice()
la4 = petersencsv.edges()
for a in la4:
	a.printArista()
print "directed: "
b4= petersencsv.directed()
print b4
"""
print "Por alguna diabolica Razon readerCSV falla con petersen.csv :v "

print "prueba: ", readerpetersenjson.ruta
print "vertices:"
lv5 = petersenjson.vertices()
for v in lv5:
	v.printVertice()
print "aristas:"
la5 = petersenjson.edges()
for a in la5:
	a.printArista()
print "directed: "
b5 = petersenjson.directed()
print b5

print "\n"

print "prueba: ", readerpetersenxml.ruta
print "vertices:"
lv6 = petersenxml.vertices()
for v in lv6:
	v.printVertice()
print "aristas:"
la6 = petersenxml.edges()
for a in la6:
	a.printArista()
print "directed: "
b6 = petersenxml.directed()
print b6
