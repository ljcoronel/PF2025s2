-- Ejercicio 1
data EA = Const Int | BOp BinOp EA EA
     deriving Show
data BinOp = Sum | Mul
     deriving Show
data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA
     deriving Show

-- Ejercicio i
evalEA :: EA -> Int
evalEA (Const n)      = n
evalEA (BOp op e1 e2) = operator op (evalEA e1) (evalEA e2)

operator :: BinOp -> Int -> Int -> Int
operator Sum = (+)
operator Mul = (*)

-- Ejercicio ii
ea2ExpA :: EA -> ExpA
ea2ExpA (Const n)      = Cte n
ea2ExpA (BOp op e1 e2) = op2expA op (ea2ExpA e1) (ea2ExpA e2)

op2expA :: BinOp -> ExpA -> ExpA -> ExpA
op2expA Sum = Suma
op2expA Mul = Prod

-- Ejercicio iii
expA2ea :: ExpA -> EA
expA2ea (Cte n)      = Const n
expA2ea (Suma e1 e2) = BOp Sum (expA2ea e1) (expA2ea e2)
expA2ea (Prod e1 e2) = BOp Mul (expA2ea e1) (expA2ea e2)

-- Ejercicio 2
data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b)
     deriving Show

-- Ejercicio i
cantidadDeHojas :: Arbol a b -> Int
cantidadDeHojas (Hoja _)       = 1
cantidadDeHojas (Nodo _ ti td) = cantidadDeHojas ti + cantidadDeHojas td

-- Ejercicio ii
cantidadDeNodos :: Arbol a b -> Int
cantidadDeNodos (Hoja _)       = 0
cantidadDeNodos (Nodo _ ti td) = 1 + cantidadDeNodos ti + cantidadDeNodos td

-- Ejercicio iii
cantidadDeConstructores :: Arbol a b -> Int
cantidadDeConstructores (Hoja _)       = 1
cantidadDeConstructores (Nodo _ ti td) = 1 + cantidadDeConstructores ti + cantidadDeConstructores td

-- Ejercicio iv
ea2Arbol :: EA -> Arbol BinOp Int
ea2Arbol (Const n)      = Hoja n
ea2Arbol (BOp op e1 e2) = Nodo op (ea2Arbol e1) (ea2Arbol e2)

-- Ejercicio 3
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
     deriving Show

-- Ejercicio i
sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT n ti td) = n + sumarT ti + sumarT td

-- Ejercicio ii
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT _ ti td) = 1 + sizeT ti + sizeT td

-- Ejercicio iii
anyT :: (a -> Bool) -> Tree a -> Bool
anyT _ EmptyT          = False
anyT p (NodeT x ti td) = p x || anyT p ti || anyT p td

-- Ejercicio iv
countT :: (a -> Bool) -> Tree a -> Int
countT _ EmptyT          = 0
countT p (NodeT x ti td) = unoSi (p x) + countT p ti + countT p td

unoSi :: Bool -> Int
unoSi True = 1
unoSi _    = 0

-- Ejercicio v
countLeaves :: Tree a -> Int
countLeaves EmptyT          = 0
countLeaves (NodeT _ ti td) = unoSiEsHoja ti td + countLeaves ti + countLeaves td

unoSiEsHoja :: Tree a -> Tree a -> Int
unoSiEsHoja EmptyT EmptyT = 1
unoSiEsHoja _      _      = 0

-- Ejercicio vi
heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT _ ti td) = 1 + heightT ti `max` heightT td

-- Ejercicio vii
inOrder :: Tree a -> [a]
inOrder EmptyT          = []
inOrder (NodeT x ti td) = inOrder ti ++ [x] ++ inOrder td

-- Ejercicio viii
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x ti td) = [x] : agruparListas (listPerLevel ti) (listPerLevel td)

agruparListas :: [[a]] -> [[a]] -> [[a]]
agruparListas []       yss      = yss
agruparListas xss      []       = xss
agruparListas (xs:xss) (ys:yss) = (xs ++ ys) : agruparListas xss yss

-- Ejercicio ix
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x ti td) = NodeT x (mirrorT td) (mirrorT ti)

-- Ejercicio x
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT          = []
levelN 0 (NodeT x _  _)  = [x]
levelN n (NodeT x ti td) = levelN (n-1) ti ++ levelN (n-1) td

-- Ejercicio xi
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x ti td) = x : laMasLarga (ramaMasLarga ti) (ramaMasLarga td)

laMasLarga :: [a] -> [a] -> [a]
laMasLarga xs ys = if length xs > length ys
                    then xs
                    else ys

-- Ejercicio xii
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x ti td) = [x] : agregarRaiz x (todosLosCaminos ti ++ todosLosCaminos td)

agregarRaiz :: a -> [[a]] -> [[a]]
agregarRaiz x []       = []
agregarRaiz x (xs:xss) = (x : xs) : agregarRaiz x xss

-- Ejercicios adicionales de Tree a
mapDobleT :: Tree Int -> Tree Int
-- PROP: Dado un arbol de enteros devuelve un arbol con el doble de cada numero.
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n ti td) = NodeT (n*2) (mapDobleT ti) (mapDobleT td)

perteneceT :: Eq a => a -> Tree a -> Bool
-- PROP: Dados un elemento y un arbol binario devuelve True si existe un elemento igual a ese en el arbol.
perteneceT _ EmptyT          = False
perteneceT e (NodeT x ti td) = e==x || perteneceT e ti || perteneceT e td

aparicionesT :: Eq a => a -> Tree a -> Int
-- PROP: Dados un elemento e y un arbol binario devuelve la cantidad de elementos del arbol que son iguales a e.
aparicionesT _ EmptyT          = 0
aparicionesT e (NodeT x ti td) = unoSi (e==x) + aparicionesT e ti + aparicionesT e td

leaves :: Tree a -> [a]
-- PROP: Dado un arbol devuelve los elementos que se encuentran en sus hojas.
leaves EmptyT          = []
leaves (NodeT x ti td) = agregarSiEsHoja x ti td (leaves ti ++ leaves td)

agregarSiEsHoja :: a -> Tree a -> Tree a -> [a] -> [a]
agregarSiEsHoja x EmptyT EmptyT xs = x:xs
agregarSiEsHoja _ _      _      xs = xs

data Dir = I | D
     deriving Show

elemAt :: [Dir] -> Tree a -> Maybe a
-- PROP: Dado una lista de direcciones y un arbol devuelve, tal vez, el elemento.
elemAt _      EmptyT          = Nothing
elemAt []     (NodeT x _  _ ) = Just x
elemAt (d:ds) (NodeT _ ti td) = chooseDirM d (elemAt ds ti) (elemAt ds td)

chooseDirM :: Dir -> Maybe a -> Maybe a -> Maybe a
chooseDirM I m _ = m
chooseDirM _ _ m = m

collectWith :: [Dir] -> Tree a -> [a]
-- PROP: Dado un camino y un arbol colecta todos los elementos hasta donde pueda o hasta llegar al final del camino, incluido.
collectWith _      EmptyT          = []
collectWith []     (NodeT x _  _ ) = [x]
collectWith (d:ds) (NodeT x ti td) = x : chooseDirL d (collectWith ds ti) (collectWith ds td)

chooseDirL :: Dir -> [a] -> [a] -> [a]
chooseDirL I xs _  = xs
chooseDirL _ _  ys = ys

pathOf :: Eq a => a -> Tree a -> Maybe [Dir]
-- PROP: Dado un elemento y un arbol delvuelve el camino hasta el elemento si puede.
pathOf e EmptyT          = Nothing
pathOf e (NodeT x ti td) = pathIf (e==x) (Just []) (pathOf e ti `choosePath` pathOf e td)

pathIf :: Bool -> Maybe [Dir] -> Maybe [Dir] -> Maybe [Dir]
pathIf True m _ = m
pathIf _    _ m = m

choosePath :: Maybe [Dir] -> Maybe [Dir] -> Maybe [Dir]
choosePath (Just ds) _         = Just (I:ds)
choosePath _         (Just ds) = Just (D:ds)
choosePath _         _         = Nothing

-- Hacer heightT = length . ramaMasLarga sin ejecutar el if (sin demostrar por casos)
-- con prop: para todo f. para todo x. para todo y. para todo b. f (if b then x else y) = if b then f x else f y
-- se demuestra por casos
-- f es estricta y b distinto de bottom

--Desafio:
--Para todo t. elem x (listPerLevel t !! n) = elem x (levelN nt)
--(!!) :: [a] -> Int -> a
--(!!) []     _ = error "out of bounds"
--(!!) (x:xs) 0 = x
--(!!) (x:xs) n = xs !! (n-1)

-- Ejercicio 4
data AppList a = Single a | Append (AppList a) (AppList a)
     deriving Show

-- Ejercicio i
lenAL :: AppList a -> Int
lenAL (Single _)     = 1
lenAL (Append li ld) = lenAL li + lenAL ld

-- Ejercicio ii
consAL :: a -> AppList a -> AppList a
consAL x al = Append (Single x) al

-- Ejercicio iii
headAL :: AppList a -> a
headAL (Single x)    = x
headAL (Append li _) = headAL li

-- Ejercicio iv
tailAL :: AppList a -> AppList a
-- PRECOND: La lista tiene mas de un elemento.
tailAL (Single x)             = error "Tiene un solo elemento"
tailAL (Append (Single _) ld) = ld
tailAL (Append li         ld) = Append (tailAL li) ld

-- Ejercicio v
snocAL :: AppList a -> a -> AppList a
snocAL al x = Append al (Single x)

-- Ejercicio vi
lastAL :: AppList a -> a
lastAL (Single x)    = x
lastAL (Append _ ld) = lastAL ld

-- Ejercicio vii
initAL :: AppList a -> AppList a
-- PRECOND: La lista tiene mas de un elemento.
initAL (Single _)             = error "Tiene un solo elemento"
initAL (Append li (Single _)) = li
initAL (Append li ld)         = Append li (initAL ld)

-- Ejercicio viii
reverseAL :: AppList a -> AppList a
reverseAL (Single x)     = Single x
reverseAL (Append li ld) = Append (reverseAL ld) (reverseAL li)

-- Ejercicio ix
elemAL :: Eq a => a -> AppList a -> Bool
elemAL x (Single y)     = x==y
elemAL x (Append li ld) = elemAL x li || elemAL x ld

-- Ejercicio x
appendAL :: AppList a -> AppList a -> AppList a
appendAL al1 al2 = Append al1 al2

-- Ejercicio xi
appListToList :: AppList a -> [a]
appListToList (Single x)     = [x]
appListToList (Append li ld) = appListToList li ++ appListToList ld

-- Ejercicio 5
data QuadTree a = LeafQ a | NodeQ (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)
     deriving Show
data Color = RGB Int Int Int
     deriving Show
type Image = QuadTree Color

-- Ejercicio i
heightQT :: QuadTree a -> Int
heightQT (LeafQ _)           = 0
heightQT (NodeQ t1 t2 t3 t4) = 1 + heightQT t1 `max` heightQT t2 `max` heightQT t3 `max` heightQT t4

-- Ejercicio ii
countLeavesQT :: QuadTree a -> Int
countLeavesQT (LeafQ _)           = 1
countLeavesQT (NodeQ t1 t2 t3 t4) = countLeavesQT t1 + countLeavesQT t2 + countLeavesQT t3 + countLeavesQT t4

-- Ejercicio iii
sizeQT :: QuadTree a -> Int
sizeQT (LeafQ _)           = 1
sizeQT (NodeQ t1 t2 t3 t4) = 1 + sizeQT t1 + sizeQT t2 + sizeQT t3 + sizeQT t4

-- Ejercicio iv
compress :: Eq a => QuadTree a -> QuadTree a
compress (LeafQ x)           = LeafQ x
compress (NodeQ t1 t2 t3 t4) = compressLeaf (compress t1) (compress t2) (compress t3) (compress t4)

compressLeaf :: Eq a => QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a
compressLeaf (LeafQ x) (LeafQ y) (LeafQ z) (LeafQ w) = if x==y && y==z && z==w
                                                        then LeafQ x
                                                        else NodeQ (LeafQ x) (LeafQ y) (LeafQ z) (LeafQ w)
compressLeaf t1        t2        t3        t4        = NodeQ t1 t2 t3 t4

-- Ejercicio v
uncompress :: QuadTree a -> QuadTree a
uncompress (LeafQ x)           = LeafQ x
uncompress (NodeQ t1 t2 t3 t4) = if hayAlgunNodo t1 t2 t3 t4
                                  then NodeQ (expandir (uncompress t1)) (expandir (uncompress t2)) (expandir (uncompress t3)) (expandir (uncompress t4))
                                  else NodeQ t1 t2 t3 t4

hayAlgunNodo :: QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a -> Bool
hayAlgunNodo t1 t2 t3 t4 = esNodo t1 || esNodo t2 || esNodo t3 || esNodo t4

esNodo :: QuadTree a -> Bool
esNodo (LeafQ _) = False
esNodo _         = True

expandir :: QuadTree a -> QuadTree a
expandir (LeafQ x) = NodeQ (LeafQ x) (LeafQ x) (LeafQ x) (LeafQ x)
expandir qt        = qt

--uncompress (NodeQ t1 t2 t3 t4) = let (ut1, ut2, ut3, ut4) = (uncompress t1 `uncompressLeaf` uncompress t2 `uncompressLeaf` uncompress t3 `uncompressLeaf` uncompress t4,
--                                                             uncompress t2 `uncompressLeaf` uncompress t1 `uncompressLeaf` uncompress t3 `uncompressLeaf` uncompress t4,
--                                                             uncompress t3 `uncompressLeaf` uncompress t1 `uncompressLeaf` uncompress t2 `uncompressLeaf` uncompress t4,
--                                                             uncompress t4 `uncompressLeaf` uncompress t1 `uncompressLeaf` uncompress t2 `uncompressLeaf` uncompress t3)
--                                  in NodeQ ut1 ut2 ut3 ut4
--uncompressLeaf :: QuadTree a -> QuadTree a -> QuadTree a
--uncompressLeaf (LeafQ x) (NodeQ _ _ _ _) = NodeQ (LeafQ x) (LeafQ x) (LeafQ x) (LeafQ x)
--uncompressLeaf q         _               = q

-- Ejercicio vi
--render :: Image -> Int -> Image

-- Ejercicio adicional
losAntecesoresDe :: Eq a => a -> Tree a -> [a]
-- PRECOND: El elemento dado existe en el arbol dado.
losAntecesoresDe x t = case posiblesAntecesores x t of
                        Nothing -> error "No existe el elemento dado"
                        Just xs -> xs

posiblesAntecesores :: Eq a => a -> Tree a -> Maybe [a]
posiblesAntecesores x EmptyT          = Nothing
posiblesAntecesores x (NodeT y t1 t2) = if x==y
                                         then Just []
                                         else posibleAgregado y (posiblesAntecesores x t1) (posiblesAntecesores x t2)

posibleAgregado :: a -> Maybe [a] -> Maybe [a] -> Maybe [a]
posibleAgregado x (Just xs) _         = Just (x:xs)
posibleAgregado x _         (Just xs) = Just (x:xs)
posibleAgregado _ _         _         = Nothing

t = NodeT 1
      (NodeT 2
            (NodeT 4 EmptyT EmptyT)
            (NodeT 5 EmptyT EmptyT))
      (NodeT 3
            (NodeT 6 EmptyT EmptyT)
            (NodeT 7
                  (NodeT 8 EmptyT EmptyT)
                  (NodeT 9 EmptyT EmptyT)))

--LEMAS a plantear:
-- Lema doblar-append-2: ¿para todo xs. para todo ys. doblar (xs ++ ys) = doblar xs ++ ys?
-- Lema asociatividad-append: ¿para todo xs. para todo ys. para todo zs. xs ++ (ys ++ zs) = (xs ++ ys) ++ zs?
