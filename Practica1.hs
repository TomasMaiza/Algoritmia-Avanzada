module Practica1 where

import Data.List

{-
1) Los siguientes códigos tienen errores, cargar el archivo 20.Practica.0.hs en el interprete de Haskell
GHCi, leer los mensajes de error y corregirlos hasta que el archivo se cargue correctamente.
-}

-- a)
regla b = case b of
    True -> "Quedate en Casa"
    False -> "Qudate en Casa"

-- b) La palabra case está reservada, entonces le cambio el nombre a la función por case'
case' [x] = []
case' (x:y:xs) = y : case' (x:xs)
case' [] = []

-- c) Lo mismo que el b)
map' f [] =  []
map' f (x:xs) = f x : map' f xs

-- d) El operador : concatena elemento:lista y devuelve una lista, no al revés. Además, no puedo tener
-- distintos tipos en una lista, por lo que no puedo incluir el elemento 'a' si incluyo 1 y 2.
listNumeros = 1 : (2 : (3 : []))

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- f) Algún drama de currificación hacía que el tail y el +x necesiten paréntesis.
addToTail x xs = map' (+x) (tail xs)

-- g) El . indica composición de funciones y estas deben ir entre paréntesis.
listmin xs = (head . sort) xs

-- h) (*) Ese (smap f) no tenía sentido, había que borrarlo.
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs

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
five :: a -> Int
five x = 5

--b
apply :: (t1 -> t) -> t1 -> t -- Asumo que viene de f (el primer paréntesis) y dsp la aplicación de f
apply f x = f x

--c
identidad :: a -> a
identidad x = x

--d
first :: (t1, t1) -> t1 
first (x, y) = x

--e
--derive :: (a -> Float) -> Float -> Float
derive f x = div (f (x+0.01) - f x) 0.1

--f
sign :: Int -> Int
sign x = if x > 0 then 1 else (if x < 0 then (-1) else 0)

--g
vabs :: Int -> Int
vabs x = if (sign x) >= 0 then x else (-x)

vabs' :: Int -> Int
vabs' x = if x >= 0 then x else (-x)

--h
pot :: Floating a => a -> a -> a
pot e b = b ** e

--i
xor :: Int -> Int -> Bool
xor x y = if x == y then False else True

--j
max3 :: Int -> Int -> Int -> Int
max3 x y z = if x > y then (if x > z then x else z) else (if y > z then y else z)

--k
swap :: (a, a) -> (a, a)
swap (x, y) = (y, x)

{-
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}

bisiesto a = if mod a 4 == 0 then (if mod a 100 == 0 then (if mod a 400 == 0 then putStrLn "Sí"
                                                                             else putStrLn "No") 
                                                     else putStrLn "Sí") 
                             else putStrLn "No"

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
fa f = f 2 + 1

fa' :: (Int -> Int) -> Int
fa' f = mod (f 1) 2

--b
fb :: Int -> (Int -> Int)
fb x y = x

fb' :: Int -> (Int -> Int)
fb' x y = x * y

--c
fc :: (Int -> Int) -> (Int -> Int)
fc f x = f x

fc' :: (Int -> Int) -> (Int -> Int)
fc' f x = f x + 1

--d
fd :: Int -> Bool
fd x = if x > 0 then True else False

fd' :: Int -> Bool
fd' x = x == 2

--e
fe :: Bool -> (Bool -> Bool)
fe x y = x || y 

fe' :: Bool -> (Bool -> Bool)
fe' x y = x && y 

--f
ff :: (Int, Char) -> Bool
ff (x, y) = x > 0

ff' :: (Int, Char) -> Bool
ff' (x, y) = x < 0

--g
fg :: (Int, Int) -> Int
fg (x, y) = x + y

fg' :: (Int, Int) -> Int
fg' (x, y) = x * y

--h
fh :: Int -> (Int, Int)
fh x = (x, 2*x)

fh' :: Int -> (Int, Int)
fh' x = (1, x)

--i
fi :: a -> Bool
fi x = True

fi' :: a -> Bool
fi' x = False

--j) a -> a
fj :: a -> a
fj x = x

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
divisors :: Int -> [Int]
divisors x = if x > 0 then [y | y <- [1..x], mod x y == 0] else []

--b
matches :: Int -> [Int] -> [Int]
matches x xs = [y | y <- xs, y == x]

--c
--cuadrupla :: Int -> [(Int, Int, Int, Int)]
cuadrupla n = [(a, b, c, d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n], a**2 + b**2 == c**2 + d**2]

--d Usa el y de la tupla como indice para sacar los elementos de la lista hasta el actual y comprobar
-- si se repite
unique :: [Int] -> [Int]
unique xs = [x | (x, y) <- zip xs [0..], notElem x (take y xs)]


{-
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}

scalarProduct xs ys = sum [x*y | (x, y) <- zip xs ys]

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
suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

--b
alguno :: [Bool] -> Bool 
alguno [] = False
alguno (x:xs) = x || alguno xs

--c
todos :: [Bool] -> Bool
todos [] = True
todos (x:xs) = x && todos xs

--d

--e
restos :: [Int] -> Int -> [Int]
restos [] n = []
restos (x:xs) n = (mod x n) : restos xs n

--f
cuadrados :: [Int] -> [Int]
cuadrados [] = []
cuadrados (x:xs) = (x*x) : cuadrados xs

--g
longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (xs:xss) = (length xs) : longitudes xss

--h
orden :: [(Int, Int)] -> [(Int, Int)]
orden [] = []
orden ((x, y):xs) = if x < 3*y then (x, y) : orden xs else orden xs

--i
pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) = if even x then x : pares xs else pares xs

--j
letras :: String -> String
letras [] = []
letras (x:xs) = if (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') then x : letras xs else letras xs

--k
masDe :: [[a]] -> Int -> [[a]]
masDe [] n = []
masDe (xs:xss) n = if length xs > n then xs : masDe xss n else masDe xss n

{-
8) Redefinir las funciones del ejercicio anterior usando foldr, map y filter.
ver su definición en https://hoogle.haskell.org/
-}

--a
suma' xs = foldr (+) 0 xs

--b
alguno' xs = if length(filter not xs) < length xs then True else False

--c
todos' xs = if length(filter not xs) == 0 then True else False

--d

--e
restos' xs n = map (`mod` n) xs  

--f
cuadrados' xs = map (\x -> x*x) xs

--g
longitudes' xss = map length xss

--h
orden' xs = filter (\(x, y) -> x < 3*y) xs

--i
pares' xs = filter even xs

--j
letras' xs = filter (\x -> (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z')) xs

--k
masDe' xss n = filter (\xs -> length xs > n) xss
