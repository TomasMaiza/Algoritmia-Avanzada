
--ALUMNO: BASUALDO IGNACIO
-- 1)
type Color = (Float, Float, Float)

mezclar :: Color -> Color -> Color
mezclar (r1, g1, b1) (r2, g2, b2) = ( (r1 + r2)/2 , (g1 + g2)/2, (b1 + b2)/2 )

{-
data Color = RGB { red::Int
                   ,green::Int
                   ,blue::Int
                 } deriving Show

mezclar :: Color -> Color -> Color
mezclar (RGB a b c) (RGB x y z) = RGB (div (a+x) 2) (div(b+y) 2) (div(c+z) 2)
-}

-- 2)
type Linea = (Int, [Char])

vacia :: Linea
vacia = (0, [])

moverIzq :: Linea -> Linea
moverIzq (cursor, linea) = if cursor == 0 then (0, linea) else (cursor - 1, linea)

moverDer :: Linea -> Linea
moverDer (cursor, linea) = if cursor == length(linea) then (cursor, linea) else (cursor + 1, linea)

moverIni :: Linea -> Linea
moverIni (cursor, linea) = (0, linea)

moverFin :: Linea -> Linea
moverFin (cursor, linea) = (length linea, linea)

insertar :: Char -> Linea -> Linea
insertar caracter (cursor, linea) = (cursor, (take cursor linea)++ [caracter] ++ (drop cursor linea))
--Pido un caracter, cursor(Int), linea(char list) = Devuelvo cursor junto con la linea que tiene el caracter insertado.
--El take va a tomar la primer parte hasta el cursor y el drop tomara lo que sigue despues del cursor

borrar :: Linea -> Linea
borrar (0, linea) = (0, linea) --caso que no haya nada
borrar (cursor, linea) = (cursor - 1, (take (cursor - 1) linea) ++ (drop cursor linea))



-- 3)
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a

headCL :: CList a -> a
headCL (CUnit v) = v
headCL (Consnoc v1 xs v2) = v1

rearmar :: CList a -> a -> CList a
rearmar xs v = case xs of
                          EmptyCL            -> (CUnit v)
                          (CUnit v1)         -> (Consnoc v1 EmptyCL v)
                          (Consnoc v1 ys v2) -> (Consnoc v1 (rearmar ys v2) v)

rearmar2 :: a -> CList a -> CList a
rearmar2 v xs = case xs of
                          EmptyCL            -> (CUnit v)
                          (CUnit v1)         -> (Consnoc v EmptyCL v1)
                          (Consnoc v1 ys v2) -> (Consnoc v (rearmar2 v1 ys) v2)

tailCL :: CList a -> CList a
tailCL (CUnit v) = EmptyCL
tailCL (Consnoc v1 xs v2) = rearmar xs v2

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnitCL :: CList a -> Bool
isCUnitCL (CUnit v) = True
isCUnitCL _ = False
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

headCL :: CList a -> a
headCL (CUnit v) = v
headCL (Consnoc v1 xs v2) = v1

tailCL :: CList a -> CList a
tailCL (CUnit v) = EmptyCL
tailCL (Consnoc v1 xs v2) = snocCL xs v2

reverseCL :: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit v) = (CUnit v)
reverseCL (Consnoc v1 xs v2) = Consnoc v2 (reverseCL xs) v1

consCL :: a -> CList a -> CList a
consCL x EmptyCL = CUnit x
consCL x (CUnit y) = Consnoc x EmptyCL y
consCL x (Consnoc y1 z y2) = Consnoc x (consCL y1 z) y2


snocCL :: CList a -> a -> CList a
snocCL EmptyCL x = CUnit x
snocCL (CUnit y) x = Consnoc y EmptyCL x
snocCL (Consnoc y1 z y2) x = Consnoc y1 (snocCL z y2) x

-----------
initsAux :: CList a -> [CList a]
initsAux EmptyCL = [EmptyCL]
initsAux lista = EmptyCL:(map (consCL (headCL lista)) (initsAux (tailCL lista)) )

listToCL :: [a] -> CList a
listToCL [] = EmptyCL
listToCL (x:xs) = consCL x (listToCL xs)


inits :: CList a -> CList (CList a)
inits lista = listToCL (initsAux lista)

lasts:: CList a -> CList (CList a)
lasts lista = listToCL (map reverseCL (initsAux (reverseCL lista)))
-------------
concatDosCL :: CList a -> CList a -> CList a
concatDosCL l1 EmptyCL = l1
concatDosCL EmptyCL l2 = l2
concatDosCL (CUnit v1) (CUnit v2) = Consnoc v1 EmptyCL v2
concatDosCL (Consnoc v11 xs v12) (CUnit v2) = Consnoc v11 (rearmar xs v12) v2
concatDosCL (CUnit v1) (Consnoc v21 xs v22) = Consnoc v1 (rearmar2 v21 xs) v22
concatDosCL (Consnoc v11 xs v12) (Consnoc v21 ys v22) = Consnoc v11 (concatDosCL (rearmar xs v12) (rearmar2 v21 ys)) v22

concatCL :: CList (CList a) -> CList a
concatCL EmptyCL = EmptyCL
concatCL (CUnit l) = l
concatCL (Consnoc l1 xs l2) = (concatDosCL((concatDosCL l1 (concatCL xs))) l2)



-- 4)

data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp
--data Maybe a = Nothing | Just a

eval :: Aexp -> Int
eval (Num n) = n
eval (Prod n1 n2) = (eval n1) * (eval n2)
eval (Div n1 n2) = div (eval n1) (eval n2)


seval :: Aexp -> Maybe Int
seval (Num n) = Just n
seval (Prod n1 n2) = case ((seval n1), (seval n2)) of
                                                      (Nothing, v2) -> Nothing
                                                      (v1, Nothing) -> Nothing
                                                      ((Just v1), (Just v2)) -> Just (v1*v2)
seval (Div n1 n2) = case ((seval n1), (seval n2)) of
                                                      (Nothing, v2) -> Nothing
                                                      (v1, Nothing) -> Nothing
                                                      ((Just v1), (Just v2)) -> if v2 == 0 then Nothing else Just (div v1 v2)

--5)
data Bin a = Hoja | Nodo (Bin a) a (Bin a)

arbol=(Nodo (Nodo (Nodo Hoja 2 Hoja) 3 (Nodo Hoja 4 Hoja)) 10 Hoja)
arbol1=(Nodo (Nodo (Nodo Hoja 2 Hoja) 3 (Nodo Hoja 4 Hoja)) 10 (Nodo (Nodo Hoja 11 Hoja) 20 (Nodo Hoja 23 Hoja)))

minimumBST :: Bin a -> a
minimumBST (Nodo Hoja a r) = a
minimumBST (Nodo l a r) = minimumBST l

maximumBST :: Bin a -> a
maximumBST (Nodo l a Hoja) = a
maximumBST (Nodo l a r) = maximumBST r

inorder :: Bin a -> [a]
inorder Hoja = []
inorder (Nodo l a r) = inorder l ++ [a] ++ inorder r

lista_ordenada :: Ord a => [a] -> Bool
lista_ordenada [] = True
lista_ordenada [x] = True
lista_ordenada (x:y:xs) = (x <= y) && (lista_ordenada (y:xs))

checkBST :: Ord a => Bin a -> Bool
checkBST Hoja = True
checkBST (Nodo l a r) = lista_ordenada(inorder(Nodo l a r))


-- 6)
data Tree a = Hoja | Nodo (Tree a) a (Tree a) deriving Show

--n es la altura y dato el numero
completo :: a -> Int -> Tree a
completo dato 0 = Hoja
completo dato n = let subarbol = (completo dato (n - 1))
                  in (Nodo subarbol dato subarbol)

balanceado :: a -> Int -> Tree a
balanceado dato 0 = Hoja
balanceado dato 1 = Nodo Hoja dato Hoja
balanceado dato n = if (mod (n - 1) 2) == 0 then let subarbol = (balanceado dato (div n 2))
                                           in (Nodo subarbol dato subarbol)
                                      else let subI = (balanceado dato (div n 2))
                                               subD = (balanceado dato ((div n 2) - 1))
                                           in (Nodo subI dato subD)
 --EJERCICIO 7)
   member :: Ord a => a -> Bin a -> Bool --member original
   member a Hoja = False
   member a (Nodo l b r) | a == b = True
                         | a < b = member a l
                         | otherwise = member a r

   member2 x Hoja = False
   member2 x t@(Nodo l b r) = member' x b t

   member' x c Hoja = c == x --devuelve true
   member' x c (Nodo l b r) | x > b = member' x c r
                            | otherwise = member' x b l



 --EJERCICIO 8)
 data Color = R | B deriving (Show,Eq)
 data RBT a = E | T Color (RBT a) a (RBT a) deriving Show

 makeBlack (T c l x r) = T B l x r

 fromOrdList xs = makeBlack (if  even(truncate (logBase 2 (fromIntegral (length xs))))
                             then fromOrdList' R xs else fromOrdList' B xs)
 --si empiezo a colorear por negro y es par llego al ultimo nivel completo con negro
 --si es impar empiezo a colorear con rojo (pero me queda la raiz roja que cambio con makeBlack)
 fromOrdList' c [] = E
 fromOrdList' c xs = let n = length xs
                         m = div n 2 --la mitad de la longitud
                         x = xs!!m --devuelve el elemento de la mitad
                         ls = take m xs --te da los primeros m elementos
                         rs = drop (m+1) xs --te da todos los elementos despues de m + 1
                         c' = if c == R then B else R --conmutador de colores
                         in T c (fromOrdList' c' ls) x (fromOrdList' c' rs)

 --fromOrdList' R [4,6,10,13,18]
--main2 = print (fromOrdList [1,2,3,4,5,6])


--EJERCICIO 9)
 lbalance :: Color -> RBT a -> a -> RBT a -> RBT a
 lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
 lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
 lbalance c l a r = T c l a r

 rbalance :: Color -> RBT a -> a -> RBT a -> RBT a
 rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
 rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
 rbalance c l a r = T c l a r

 ins x E = T R E x E
 ins x (T c l y r) | x < y = lbalance c (ins x l) y r
                   | x > y = rbalance c l y (ins x r)
                   | otherwise = T c l y r


 --EJERCICIO 10)
   type Rank = Int
   data Heap a = H | N Rank a (Heap a) (Heap a) deriving (Show,Eq)

   rank :: Heap a -> Rank
   rank H = 0
   rank (N r _ _ _) = r

   makeH x a b = if rank a >= rank b then N (rank b + 1) x a b
    else N (rank a + 1) x b a

   merge :: Ord a => Heap a -> Heap a -> Heap a
   merge h1 H = h1
   merge H h2 = h2
   merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) =
    if x <= y then makeH x a1 (merge b1 h2)
      else makeH y a2 (merge h1 b2)
   --lo voy haciendo por pasadas, voy agarrando los elementos de a pares   --hasta que me quede un solo heap, voy a hacer log 2 length pasadas
   pares [] = []
   pares [x] = [x]
   pares (x:y:xs) = (merge x y): pares xs --hago una pasada, y aplico merge cada dos
   g [] = H
   g [h] = h
   g ys = g (pares ys) --aplico pares hasta que quede la lista con un solo elemento

   fromList [] = H
   fromList xs = let hs = map (\x->N 0 x H H) xs --convierto toda la lista y convertirlo a una lista de heaps unitarios
            in g hs
