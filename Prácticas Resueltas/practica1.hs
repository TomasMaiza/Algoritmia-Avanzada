module Practica0 where

import Data.List
import Data.Char

{-
1) Los siguientes códigos tienen errores, cargar el archivo 20.Practica.0.hs en el interprete de Haskell
GHCi, leer los mensajes de error y corregirlos hasta que el archivo se cargue correctamente.
-}

-- a)
regla b = case b of
    True -> "Quedate en Casa"
    False -> "Qudate en Casa"

-- b)
case' [x]         =  []
case' (x:y:xs)      =  y : case' (x:xs)
case' []          =  []

-- c)
map' f []        =  []
map' f (x:xs)     =  f x : map' f xs

-- d)
listNumeros = 1 : (2 : (3 : []))

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- f)
addToTail x xs = map (+x tail xs)

-- g)
listmin xs = (head . sort) xs

-- h) (*)
{-smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = (f x) : (smap (smap f xs))-}

{-
2. Definir las siguientes funciones y determinar su tipo:

a) five, que dado cualquier valor, devuelve 5

b) apply, que toma una función y un valor, y devuelve el resultado de
aplicar la función al valor dado

c) identidad, la función identidad

d) first, que toma un par ordenado, y devuelve su primera componente

e) derive, que aproxima la derivada de una función dada en un punto dado

f) sign, la función signo

g) vabs, la función valor absoluto (usando sign y sin usarla)

h) pot, que toma un entero y un número, y devuelve el resultado de
elevar el segundo a la potencia dada por el primero

i) xor, el operador de disyunción exclusiva

j) max3, que toma tres números enteros y devuelve el máximo entre llos

k) swap, que toma un par y devuelve el par con sus componentes invertidas
-}

--a
five :: Int -> Int
five x = 5

--b
apply :: (t1 -> t2) -> t1 -> t2
apply f x = f x

--c
identidad :: Int -> Int
identidad x = x

--d
first :: (a, b) -> a
first (x, y) = x

--e

--f
sign :: Int -> Int
sign x | x > 0 = 1
       | x == 0 = 0
       | otherwise = -1

--g
vabs x = if (sign x >=0) then x else -x

vabs' x = if (x >= 0) then x else -x

--h
pot x 0 = 1
pot x y = x * (pot x (y-1))

--i
xor x y = if (x == y) then False else True

--j
max3 a b c = if (a > b) then (if a > c then a else c) else (if b > c then b else c)

--k
swap (x, y) = (y, x)

{-
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}

bis x = if (mod x 4 == 0) then (if mod x 100 == 0 then (if mod x 400 == 0 then True else False) else True) else False

{-
4) Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:
a) (Int -> Int) -> Int
b) Int -> (Int -> Int)
c) (Int -> Int) -> (Int -> Int)
d) Int -> Bool
e) Bool -> (Bool -> Bool)
f) (Int,Char) -> Bool
g) (Int,Int) -> Int
h) Int -> (Int,Int)
i) a -> Bool
j) a -> a
-}

--a
fa :: (Int -> Int) -> Int
fa g = (g 2) + 1
fa' g = (g 3) + 4

--b
fb :: Int -> (Int -> Int) -- es lo mismo que Int -> Int -> Int porque -> asocia a derecha
fb x y = x*y
fb' x y = x+y

--c

--d
fd :: Int -> Bool
fd x = x==0
fd' x = x<0

--e
fe :: Bool -> (Bool -> Bool)
fe a b = a
fe' a b = b

--f
ff :: (Int, Char) -> Bool
ff (a, b) = a /= 0 && b /= 'c'
ff' (a, b) = a < 0 || b /= 'j'

--g
fg :: (Int, Int) -> Int
fg (a, b) = a + b
fg' (a, b) = a * b

--h
fh :: Int -> (Int, Int)
fh x = (x, x+1)
fh' x = (x*2, x-4)

--i
fi :: a -> Bool
fi a = False
fi' a = True

--j
fj :: a -> a
fj a = a

{-
5) Definir las siguientes funciones usando listas por comprensión:

a) 'divisors', que dado un entero positivo 'x' devuelve la
lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)

b) 'matches', que dados un entero 'x' y una lista de enteros descarta
de la lista los elementos distintos a 'x'

c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
donde 0 <= a, b, c, d <= 'n'

(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
unique :: [Int] -> [Int]
-}

--a
divisors x | x<=0 = []
           | otherwise = [y | y <- [1..x], (mod x y == 0)]

--b
matches x ls = [y | y <- ls, x/=y]

--c
--cuadrupla n = [(a, b, c, d) | (a, b, c, d) <- zip [0..n]]

--d
unique xs = [x | (x, i) <- zip xs [1..l], not (elem x (drop i xs))] where l = length xs

{-
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}

scalarProduct [] [] = 0
scalarProduct xs [] = 0
scalarProduct [] ys = 0
scalarProduct xs ys = sum [(x * y) | (x, y) <- zip xs ys]

{-
7) Sin usar funciones definidas en el
preludio, defina recursivamente las siguientes funciones y
determine su tipo más general:

a) 'suma', que suma todos los elementos de una lista de números

b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario

c) 'todos', que devuelve True si todos los elementos de
una lista de valores booleanos son True, y False en caso
contrario

d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales

e) 'restos', que calcula la lista de los restos de la
división de los elementos de una lista de números dada por otro
número dado

f) 'cuadrados', que dada una lista de números, devuelva la
lista de sus cuadrados

g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes

h) 'orden', que dada una lista de pares de números, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda

i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares

j) 'letras', que dada una lista de caracteres, devuelve la
lista de aquellos que son letras (minúsculas o mayúsculas)

k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' -}

--a
suma [] = 0
suma (l:ls) = l + (suma ls)

--b
alguno [] = False
alguno (l:ls) = if (l == True) then True else alguno ls

--c
todos [] = False
todos (l:ls) = if (l == False) then False else (if ls==[] then True else todos ls)

--d
codes [] = []
codes (l:ls) = ((ord l) - 65) : codes ls

--e
restos [] x = []
restos (l:ls) x = mod l x : (restos ls x)

--f
cuadrados [] = []
cuadrados (x:xs) = (x * x) : cuadrados xs

--g
longitudes [] = []
longitudes [[]] = [0]
longitudes (xs:xss) = length xs : longitudes xss

--h
orden [] = []
orden ((a, b):xs) = if a < (b*3) then (a, b) : orden xs else orden xs

--i
pares [] = []
pares (x:xs) = if mod x 2 == 0 then x : pares xs else pares xs

--j
letras [] = []
letras (x:xs) = if elem x ['A'..'Z'] || elem x ['a'..'z'] then x : letras xs else letras xs

--k
masDe [] n = []
masDe (xs:xss) n = if length xs > n then xs : masDe xss n else masDe xss n

{-
8) Redefinir las funciones del ejercicio anterior usando foldr, map y filter.
ver su definición en https://hoogle.haskell.org/
-}

--a
suma' xs = foldr (\x y -> x + y) 0 xs

--b
alguno' xs = if (filter (==True) xs) == [] then False else True

--c
todos' xs = if (filter (==False) xs) == [] then True else False

--d
codes' xs = map ord xs

--e
restos' xs y = map (`mod` y) xs

--f
cuadrados' xs = map (\x -> x * x) xs

--g
longitudes' xss = map length xss

--h
orden' xs = filter (\(a, b) -> a < b*3) xs

--i
pares' xs = filter even xs

--j
letras' xs = filter (\x -> elem x ['A'..'Z'] || elem x ['a'..'z']) xs

--k
masDe' xss n = filter (\xs -> length xs > n) xss
