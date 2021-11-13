{-
1. El modelo de color RGB es un modelo aditivo que tiene al rojo, verde y azul como colores
primarios. Cualquier otro color se expresa en t´erminos de las proporciones de estos tres colores que
es necesario combinar en forma aditiva para obtenerlo. Dichas proporciones caracterizan a cada
color de manera biun´ıvoca, por lo que usualmente se utilizan estos valores como representaci´on de
un color.
Definir un tipo Color en este modelo y una funci´on mezclar que permita obtener el promedio
componente a componente entre dos colores.
-}

type Red = Int
type Green = Int
type Blue = Int
type Color1 = (Red, Green, Blue)
--data Color = RGB Int Int Int deriving Show
{-
data Color = RGB {
                red:: Int
                green:: Int
                blue:: Int
            } deriving Show
-}
mezclar :: Color1 -> Color1 -> Color1
mezclar (a, b, c) (x, y, z) = (div (a+x) 2, div (b+y) 2, div (c+z) 2)
--mezclar (RGB a b c) (RGB x y z) = RGB (div (a+x) 2) (div (b+y) 2) (div (c+z) 2)

{-
Consideremos un editor de líneas simple. Supongamos que una Línea es una secuencia de
caracteres c1, c2, . . . , cn junto con una posición p, siendo 0 <= p <= n, llamada cursor (consideraremos
al cursor a la izquierda de un caracter que será borrado o insertado, es decir como el cursor de la
mayoría de los editores). Se requieren las siguientes operaciones sobre Líneas:
vacía :: Línea
moverIzq :: Línea → Línea
moverDer :: Línea → Línea
moverIni :: Línea → Línea
moverFin :: Línea → Línea
insertar :: Char → Línea → Línea
borrar :: Línea → Línea
La descripción informal es la siguiente: (1) la constante vacía denota la línea vacía, (2) la operación moverIzq mueve el cursor una posición a la izquierda
(siempre que ello sea posible), (3) análogamente para moverDer , (4) moverIni mueve el cursor al comienzo de la línea, (5) moverFin
mueve el cursor al final de la línea, (6) la operación borrar elimina el caracterer que se encuentra
a la izquierda del cursor, (7) insertar agrega un caracter en el lugar donde se encontraba el cursor
y lo mueve una posición a la derecha.
Definir un tipo de datos Línea e implementar las operaciones dadas.
-}

type Cadena = [Char]

type Linea4  = ([Char], [Char])
--Linea4

print' (xs, ys) = reverse xs ++ ys

vacia' = ([], [])

moverIzq' ([], ys) = ([], ys)
moverIzq' (x:xs, ys) = (xs, x:ys)

moverDer' (xs, []) = (xs, [])
moverDer' (xs, y:ys) = (y:xs, ys)

moverIni' ([], ys) = ([], ys)
moverIni' (xs, ys) = ([], reverse xs ++ ys)

moverFin' (xs, []) = (xs, [])
moverFin' (xs, ys) = (reverse ys ++ xs, [])

insertar' c (xs, ys) = print' (c:xs, ys)

borrar' (x:xs, ys) = print' (xs, ys)

type Linea2 = (Int, [Char])
--Linea2

vacia = (0, "")

punteroValido p xs = not (p > length xs || p < 0)

moverIzq (0, xs) = (0, xs)
moverIzq (p, xs) | not (punteroValido p xs) = error "El puntero no es correcto"
                 | otherwise = (p-1, xs)

moverDer (p, xs) | p == length xs = (p, xs)
                 | not (punteroValido p xs) = error "El puntero no es correcto"
                 | otherwise = (p+1, xs)

moverIni (p, xs) | not (punteroValido p xs) = error "El puntero no es correcto"
                 | otherwise = (0, xs)

moverFin (p, xs) | not (punteroValido p xs) = error "El puntero no es correcto"
                 | otherwise = (length(xs), xs)

insertar :: Char -> Linea2 -> Linea2

ins c 0 xs = c:xs
ins c p (x:xs) = x: ins c (p-1) xs
insertar c (p, xs) = (p+1, ins c p xs)

bor p xs = take (p-1) xs ++ drop p xs
borrar (0, xs) = (0, xs)
borrar (p, xs) = (p-1, (bor p xs))

{-
3. Dado el tipo de datos
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a
a) Implementar las operaciones de este tipo algebraico teniendo en cuenta que:
Las funciones de acceso son headCL, tailCL, isEmptyCL,isCUnit.
headCL y tailCL no están definidos para una lista vacía.
headCL toma una CList y devuelve el primer elemento de la misma (el de más a la
izquierda).
tailCL toma una CList y devuelve la misma sin el primer elemento.
isEmptyCL aplicado a una CList devuelve True si la CList es vacía (EmptyCL) o False
en caso contrario.
isCUnit aplicado a una CList devuelve True si la CList tiene un solo elemento (CUnit a)
o False en caso contrario.
b) Definir una función reverseCL que toma una CList y devuelve su inversa.
c) Definir una función inits que toma una CList y devuelve una CList con todos los posibles
inicios de la CList.
d) Definir una función lasts que toma una CList y devuelve una CList con todas las posibles
terminaciones de la CList.
e) Definir una función concatCL que toma una CList de CList y devuelve la CList con todas ellas
concatenadas
-}

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

{-
[] --> EmptyCL
[1] --> CUnit 1
[1, 2] --> Consnoc 1 EmptyCL 2
[1, 2, 3] --> Consnoc 1 (CUnit 2) 3
[1, 2, 3, 4, 5] --> Consnoc 1 (Consnoc 2 (CUnit 3) 4) 5
-}

--a

headCL (CUnit x) = x
headCL (Consnoc p m l) = p

tailCL (CUnit x) = EmptyCL
tailCL (Consnoc p EmptyCL l) = CUnit l
tailCL (Consnoc p m l) = Consnoc (headCL m) (tailCL m) l

isEmptyCL (EmptyCL) = True
isEmptyCL (CUnit x) = False
isEmptyCL (Consnoc p m l) = False

isCUnit (CUnit a) = True
isCUnit (EmptyCL) = False
isCUnit (Consnoc p m l) = False

--b

reverseCL (EmptyCL) = EmptyCL
reverseCL (CUnit a) = CUnit a
reverseCL (Consnoc p m l) = Consnoc l (reverseCL m) p

--c

inits (EmptyCL) = EmptyCL
inits (CUnit a) = Consnoc EmptyCL EmptyCL (CUnit a)
--inits (Consnoc p m l) = Consnoc()

--cons :: a -> CList a -> CList a --inserta un elemento al principio
--snoc :: CList a -> a -> CList a --inserta un elemento al final
--append' :: CList a -> CList a -> CList a --concatena dos listas CL

--inits [1, 2, 3, 4] --> [[], [1], [1, 2], [1, 2, 3], [1, 2, 3, 4]]
--lasts [1, 2, 3, 4] --> [[], [4], [3, 4], [2, 3, 4], [1, 2, 3, 4]]
--concatCL [[], [4], [3, 4], [2, 3, 4], [1, 2, 3, 4]] --> [4, 3, 4, 2, 3, 4, 1, 2, 3, 4]

{-
4. Dado el siguiente tipo algebraico:
data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp
a) Defina un evaluador eval :: Aexp → Int. ¿C´omo maneja los errores de divisi´on por 0?
b) Defina un evaluador seval :: Aexp → Maybe Int.
-}

data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp deriving Show

--a

eval (Num x) = x
eval (Prod e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = div (eval e1) (eval e2)

--b

seval :: Aexp -> Maybe Int

maybeToInt (Just x) = x

seval (Num x) = Just x
seval (Prod e1 e2) = Just (maybeToInt (seval e1) * maybeToInt (seval e2))
seval (Div _ (Num 0)) = Nothing
seval (Div e1 e2) = Just (div (maybeToInt (seval e1)) (maybeToInt (seval e2)))

{-
seval (Num x) = Just x
seval (Prod e1 e2) = case (seval e2) of
                          Nothing -> Nothing
                          Just x -> case (seval e1) of
                                         Nothing -> Nothing
                                         Just y -> Just (y * x)
seval (Div e1 e2) = case (seval e2) of
                         -- (just 0) -> Nothing
                         Nothing -> Nothing
                         Just x -> case (seval e1) of
                                        Nothing -> Nothing
                                        Just y -> if(x==0) then Nothing else Just (div y x)


seval (Div e1 e2) = case (seval e2, seval e1) of
                         (Nothing, _) -> Nothing
                         (_, Nothing) -> Nothing
                         (Just x, Just y) -> if(x==0) then Nothing else Just (div y x)

-}

{-
5. Definir las siguientes funciones sobre árboles binarios de búsqueda (bst):
  a. maximum :: BST a → a, que calcula el máximo valor en un bst.
  b. checkBST :: BST a → Bool, que chequea si un árbol binario es un bst.
-}

data BST a = Hoja | Nodo (BST a) a (BST a) deriving Show

bin = Nodo (Nodo (Nodo Hoja 2 Hoja) 3 (Nodo Hoja 5 Hoja)) 5 (Nodo Hoja 7 (Nodo Hoja 8 Hoja))
noBin = Nodo (Nodo (Nodo Hoja 2 Hoja) 3 (Nodo Hoja 5 Hoja)) 5 (Nodo Hoja 4 (Nodo Hoja 8 Hoja))

--a

maximum' :: BST a -> a
maximum' (Nodo l a Hoja) = a
maximum' (Nodo l a r) = maximum' r

minimum' :: BST a -> a
minimum' (Nodo Hoja a r) = a
minimum' (Nodo l a r) = minimum' l

--b

valor (Nodo l a r) = a

checkBST :: BST a -> Bool
checkBST (Hoja) = True
--checkBST (Nodo l a r) = ((maximum' l) <= a) && ((minimum' r) > a)

{-
6. Si un árbol binario es dado como un nodo con dos subárboles idénticos se puede aplicar
la técnica sharing, para que los subárboles sean representados por el mismo árbol. Definir las
siguientes funciones de manera que se puedan compartir la mayor cantidad posible de elementos
de los árboles creados:
a) completo :: a → Int → Tree a, tal que dado un valor x de tipo a y un entero d, crea un árbol
binario completo de altura d con el valor x en cada nodo.
b) balanceado :: a → Int → Tree a, tal que dado un valor x de tipo a y un entero n, crea un árbol
binario balanceado de tamaño n, con el valor x en cada nodo.
-}

data Bin a = Hoja' | Nodo' (Bin a) a (Bin a) deriving Show

--a

--completo :: a → Int → Bin a
completo x 0 = Nodo' Hoja' x Hoja'
completo x d = Nodo' (completo x (d-1)) x (completo x (d-1))

--b

{-
Un árbol binario balanceado es un árbol binario en el cual las alturas de los dos subárboles de todo nodo difiere a lo sumo en 1.
El balance de un nodo en un árbol binario se define como la altura de su subárbol izquierdo menos la altura de su subárbol derecho.
-}

balanceado x 0 = Nodo' Hoja' x Hoja'
balanceado x n = if (mod n 2 == 0) then Nodo' (balanceado x (n-1)) x Hoja' else Nodo' Hoja' x (balanceado x (n-1))

{-
7. La definición de member dada en teoría (la cual determina si un elemento está en un bst)
realiza en el peor caso 2 ∗ d comparaciones, donde d es la altura del árbol. Dar una definición
de member que realice a lo sumo d + 1 comparaciones. Para ello definir member en términos de
una función auxiliar que tenga como parámetro el elemento candidato, el cual puede ser igual al
elemento que se desea buscar (por ejemplo, el último elemento para el cual la comparación de
a <= b retornó True) y que chequee que los elementos son iguales sólo cuando llega a una hoja del
árbol.
-}

{-
member x H = False
member x (N l b r) | x == b = True
                   | x < b = member x l
                   | otherwise = member x r-}

ex = (Nodo (Nodo (Nodo Hoja 15 (Nodo Hoja 20 Hoja)) 3 (Nodo Hoja 7 Hoja)) 4 (Nodo Hoja 5 Hoja))

member x Hoja = False
member x t@(Nodo l b r) = member' x b t

member' x c Hoja = x == c
member' x c (Nodo l b r) | x <= b = member' x b l
                         | otherwise = member' x b r

{-
8. Definir una función fromOrdList :: [a ] → RBT a, que cree un red black tree a partir de una
lista ordenada sin elementos repetidos. La función debe ser de orden O(n).
-}

data Color = R | B deriving (Show, Eq)
data RBT a = E | T Color (RBT a) a (RBT a) deriving Show

makeBlack (T c l x r) = T B l x r

fromOrdList xs = makeBlack (if even(truncate(logBase 2 (fromIntegral(length xs)))) then fromOrdList' R xs else fromOrdList' B xs)

fromOrdList' c [] = E
formOrdList' c xs = let n = length xs
                        m = div n 2
                        x = xs!!m --devuelve el elemento en la posición m
                        ls = take m xs
                        rs = drop (m+1) xs
                        c' = if c == R then B else R
                     in T c (fromOrdList' c' ls) x (fromOrdList' c' rs)

{-
10. Definir una función fromList :: [a ] → Heap a, que cree un leftist heap a partir de una lista,
convirtiendo cada elemento de la lista en un heap de un solo elemento y aplicando la función merge
hasta obtener un solo heap. Aplicar la función merge (log n) veces, donde n es la longitud de la
lista que recibe como argumento la función, de manera que fromList sea de orden O(n).
-}

type Rank = Int
data Heap a = H | N Rank a (Heap a) (Heap a) deriving (Show, Eq)

merge h1 H = h1
merge H h2 = h2
merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) = if x <= y then makeH x a1 (merge b1 h2) else makeH y a2 (merge h1 b2)

rank H = 0
rank (N r _ _ _) = r

makeH x a b = if rank a > rank b then N (rank b + 1) x a b else N (rank a + 1) x b a

pares [] = []
pares [x] = [x]
pares(x:y:xs) = (merge x y) : pares xs

g [] = H
g [h] = h
g ys = g (pares ys)

fromList [] = H
fromList xs = let hs = map (\x-> N 0 x H H) xs in g hs
