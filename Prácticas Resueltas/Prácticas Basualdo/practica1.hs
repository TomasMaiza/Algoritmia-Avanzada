module Practica0 where

import Data.List

--ALUMNO:BASUALDO IGNACIO


--1)
-- a)
--El error era que el constructor case usa un bloque de posibilidades(tiene que estar alineadas y despues del of)
regla b = case b of
                    True -> "Quedate en Casa"
                    False -> "Quedate en Casa"

-- b)
-- El error era que "case" estaba definido como una funcion del programa
cases [x]         =  []
cases (x:y:xs)      =  y : cases (x:xs)
cases []          =  []

-- c)
-- El error estaba en que usaba "map" como nombre de una variable
mimap f []        =  []
mimap f (x:xs)     =  f x : mimap f xs

-- d)
-- El error era que habia una a y que habia que modificar el orden de los parentesis
listNumeros = 1 : (2 : (3 : []))

-- e)
[] ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- f)
-- El errror era que habia que agregar unos parentesis
addToTail x xs = map (+x) (tail xs)

-- g)
-- Le agregue unos parentesis al head y parece no detectar error
listmin xs = head (sort xs)
-- listmnin xs = (head . sort) xs
-- minimol xs = head $ sort xs

-- h) (*)

smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs

-- smap :: (a -> b ) -> [a] -> []


---------------------------------------------------------------------------------


-- 2-a) five :: Num a => t -> a
five x = 5

-- 2-b) apply :: (a -> b) -> a -> b
apply f x = f x

-- 2-c) ident :: a -> a
ident x = x

-- 2-d) first :: (a,b) -> a
first (x,y) = x

-- 2-e) derive :: (Fractional a, Fractional a1) => (a1 -> a) -> a1 -> a
derive f x = (f x+0.1 - f x) / 0.1

-- 2-f) sign :: (Num a, Ord a, Num b) => a -> b
sign x | x < 0   = -1
       | x == 0  = 0
       | otherwise = 1

-- 2-g) vabs1 :: (Num a, Ord a) => a -> a
--      vabs2 :: (Num a, Ord a) => a -> a
vabs1 x = x * sign x

vabs2 x | x <= 0     = -x
        | otherwise  = x

-- 2-h) pot :: (Integral b, Num a) => b -> a -> a
pot x y = y ^ x

-- 2-i) xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True

-- 2-j) max3 :: Ord a => a -> a -> a -> a
max3 a b c = max a (max b c)
--max3 [x,y,z] = maximum [x,y,z]


-- 2-k) swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

--------------------------------------------------------------------------------------------------------
-- 3) bisiesto :: (Integral a) => a -> Bool

bisiesto año =(((mod año 4 == 0) && (mod año 100 /= 0)) ||(mod año 400 == 0))
{--
bisiesto x | ((mod x 4) /= 0)                                 = False
           | and [(mod x 100) == 0, (mod (div x 100) 4) /= 0] = False
           | otherwise                                        = True

--}


-----------------------------------------------------------------------------------------------------

-- 4)
uno::(Int -> Int) -> Int
uno f = (f 2) + 1

dos::Int -> (Int -> Int)
dos a = (\x ->x+2)

tres::(Int -> Int) -> (Int -> Int)
tres f = (\x -> (f x) + 1)

cuatro::Int -> Bool
cuatro x = if x>0 then True else False

cinco::Bool -> (Bool -> Bool)
cinco True True = False

seis::(Int,Char) -> Bool
seis (x,c) = if x>0 && c == 'a' then True else False

siete::(Int,Int) -> Int
siete(x,y) = x+y

ocho::Int -> (Int,Int)
ocho x = (x,x + 1)

nueve::a -> Bool
nueve x = True

diez::a -> a
diez x = x


-----------------------------------------------------------------------------------------------------
-- 5)
--a)
divisors n = [x | x <- [1..n], mod n x == 0]

--b)
matches n lista = [x | x <- lista, x == n]

--c)
cuadrupla n = [(a,b,c,d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n], a*a + b*b == c*c + d*d]
{-
solucion que pasaron por el grupo de info
cuadrupla n = [(a,b,c,d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n], a*a + b*b == c*c + d*d]
-}
--d)

unique xs = [x | (x,y) <- zip xs [0..], x `notElem` (take y xs)) ]

--------------------------------------------------------------------------------------------------------
-- 6)
productupla (x,y) = x * y
scalarProduct x y = sum (map productupla (zip x y))

-----------------------------------------------------------------------------------------------------

-- 7-a)suma:: Num a => [a] -> a
suma [] = 0
suma (x:xs) = x + suma xs

-- 7-b)alguno::[Bool] -> Bool
alguno [] = False
alguno (x:xs) = x || alguno xs

-- 7-c)todos::[Bool] -> Bool
todos [] = True
todos (x:xs) = x && todos xs

-- 7-d) codes:: [Char] -> [Int]

codes [] = []
codes (x:xs) = ord x ++ codes xs


-- 7-e) restos:: Integral a => [a] -> a -> [a]
restos [] numero = []
restos (x:xs) numero = (mod x numero) : (restos xs numero)

-- 7-f) cuadrados:: Num a => [a] -> [a]
cuadrados [] = []
cuadrados (x:xs) = (x * x) : cuadrados xs

-- 7-g) longitudes:: [[a]] -> [Int]
longitudes [] = []
longitudes (x:xs) = length x : longitudes xs

-- 7-h) orden:: (Ord a, Num a) => [(a,a)] -> [(a,a)]
orden [] = []
orden ((x,y):xs) = if x < 3 * y then (x,y):orden xs else orden xs

-- 7-i) pares:: (Integral a) => [a] -> [a]
pares [] = []
pares (x:xs) = if mod x 2 == 0 then x:pares xs else pares xs

-- 7-j) letras:: [Char] -> [Char]
letras [] = []
letras (x:xs) = if ((x >= 'a') && (x <= 'z')) || ((x >= 'A') && (x <= 'Z')) then x:letras xs else letras xs

-- 7-k) masDe:: (Ord t) => [[a]] -> Int -> [[a]]
masDe [] largo = []
masDe (x:xs) largo = if length x > largo then x:masDe xs largo else masDe xs largo

-----------------------------------------------------------------------------------------------
--8)
auxa x y = x + y
suma' l = foldr auxa 0 l

auxb x y = x || y
alguno' l = foldr auxb False l

auxc x y = x && y
todos' l = foldr auxb True l --en caso de lista vacia devuelve true.

auxd x = ord x
codes' l = map auxd l

auxe n x = x `mod` n
restos' l n = map (auxe n) l

auxf x = x^2
cuadrados' l = map auxf l

auxg x = length x
longitudes' l = map auxg l

auxh (x, y) = x < 3*y
orden' l = filter auxh l

auxi x = x `mod` 2 == 0
pares' l = filter auxi l

auxj x = ord x > 64 && ord x < 123
letras' l = filter auxj l

auxk n x = length x > n
masDe' l n = filter (auxk n) l
