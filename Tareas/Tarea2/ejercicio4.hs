--Ejercicio4: Funcion que nos regresa n primos en una lista
primos :: Integer -> [Integer]
primos 0 = []
primos  n  = [x | x<-[1..n],esPrimo (x)]
--Funcion auxiliar que nos dice si un numero es primo
esPrimo :: Integer -> Bool
esPrimo  n = if (length (divisoresDe n) ==2)
			then True
			else False
--Funcion auxiliar que nos da una lista con los divisores del numero que le pasemos
divisoresDe :: Integer -> [Integer]
divisoresDe n | n > 0 = [d | d <- [1..n], n `mod` d == 0]