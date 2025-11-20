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
evalEA (BOp op e1 e2) = evalBOp op (evalEA e1) (evalEA e2)

evalBOp :: BinOp -> Int -> Int -> Int
evalBOp Sum = (+)
evalBOp Mul = (*)

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
ramaMasLarga (NodeT x ti td) = x : maxBy length (ramaMasLarga ti) (ramaMasLarga td)

maxBy :: (a -> Int) -> a -> a -> a
maxBy f x y = if f x > f y then x else y

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
lenAL (Append xs ys) = lenAL xs + lenAL ys

-- Ejercicio ii
consAL :: a -> AppList a -> AppList a
consAL x al = Append (Single x) al

-- Ejercicio iii
headAL :: AppList a -> a
headAL (Single x)    = x
headAL (Append xs _) = headAL xs

-- Ejercicio iv
tailAL :: AppList a -> AppList a
-- PRECOND: La lista tiene mas de un elemento.
tailAL (Single x)             = error "Tiene un solo elemento"
tailAL (Append (Single _) ys) = ys
tailAL (Append xs         ys) = Append (tailAL xs) ys

-- Ejercicio v
snocAL :: AppList a -> a -> AppList a
snocAL al x = Append al (Single x)

-- Ejercicio vi
lastAL :: AppList a -> a
lastAL (Single x)    = x
lastAL (Append _ ys) = lastAL ys

-- Ejercicio vii
initAL :: AppList a -> AppList a
-- PRECOND: La lista tiene mas de un elemento.
initAL (Single _)             = error "Tiene un solo elemento"
initAL (Append xs (Single _)) = xs
initAL (Append xs ys)         = Append xs (initAL ys)

-- Ejercicio viii
reverseAL :: AppList a -> AppList a
reverseAL (Single x)     = Single x
reverseAL (Append xs ys) = Append (reverseAL ys) (reverseAL xs)

-- Ejercicio ix
elemAL :: Eq a => a -> AppList a -> Bool
elemAL x (Single y)     = x==y
elemAL x (Append xs ys) = elemAL x xs || elemAL x ys

-- Ejercicio x
appendAL :: AppList a -> AppList a -> AppList a
appendAL al1 al2 = Append al1 al2

-- Ejercicio xi
appListToList :: AppList a -> [a]
appListToList (Single x)     = [x]
appListToList (Append xs ys) = appListToList xs ++ appListToList ys

-- Ejercicio 5
data QuadTree a = LeafQ a | NodeQ (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)
     deriving Show
data Color = RGB Int Int Int
     deriving Show
type Image = QuadTree Color

-- Ejercicio i
heightQT :: QuadTree a -> Int
heightQT (LeafQ _)           = 1
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
uncompress t = expandir (heightQT t) t

expandir :: Int -> QuadTree a -> QuadTree a
expandir 0 t                   = t
expandir n (LeafQ x)           = NodeQ (expandir (n-1) (LeafQ x)) (expandir (n-1) (LeafQ x)) (expandir (n-1) (LeafQ x)) (expandir (n-1) (LeafQ x))
expandir n (NodeQ t1 t2 t3 t4) = NodeQ (expandir (n-1) t1) (expandir (n-1) t2) (expandir (n-1) t3) (expandir (n-1) t4)

-- Ejercicio vi
render :: Image -> Int -> Image
render i n = expandir (logBase 4 n) i

-- Ejercicio adicional
data Objeto = Moneda | Pelusa
data Dungeon = Cueva | Habitacion [Objeto] Dungeon Dungeon

objs :: Dungeon -> [Objeto]
objs Cueva                 = []
objs (Habitacion os d1 d2) = os ++ objs d1 ++ objs d2

excavar :: Dungeon -> Dungeon
excavar Cueva                 = Habitacion [] Cueva Cueva
excavar (Habitacion os d1 d2) = Habitacion (doblar os) (excavar d1) (excavar d2)

doblar :: [Objeto] -> [Objeto]
doblar []     = []
doblar (o:os) = doblarObj o ++ doblar os

doblarObj :: Objeto -> [Objeto]
doblarObj Moneda = [Moneda, Moneda]
doblarObj o      = [o]

Prop: ¿objs . excavar = doblar . objs?
Dem: por ppio de ext.
¿para todo d. (objs . excavar) d = (doblar . objs) d?
por (.)
¿para todo d. objs (excavar d) = doblar (objs d)?
sea d'. Por ppio de ind. en la estructura de d':
CB d' = Cueva) ¿objs (excavar Cueva) = doblar (objs Cueva)?
CI d' = Habitacion os d1 d2) HI1) ¡objs (excavar d1) = doblar (objs d1)!
                             HI2) ¡objs (excavar d2) = doblar (objs d2)!
                             TI) ¿objs (excavar (Habitacion os d1 d2)) = doblar (objs (Habitacion os d1 d2))?
Dem CB:
Lado izq:
  objs (excavar Cueva)
=                                (excavar.1)
  objs (Habitacion [] Cueva Cueva)
=                                (objs.2)
  [] ++ objs Cueva ++ objs Cueva
=                                (objs.1)
  [] ++ [] ++ objs Cueva
=                                (objs.1)
  [] ++ [] ++ []
=                                (++.1)
  [] ++ []
=                                (++.1)
  []
Lado der:
  doblar (objs Cueva)
=                                (objs.1)
  doblar []
=                                (doblar.1)
  []  -- VALE EL CASO
Dem CI:
Lado izq:
  objs (excavar (Habitacion os d1 d2))
=                                (excavar.2)
  objs (Habitacion (doblar os) (excavar d1) (excavar d2))
=                                (objs.2)
  doblar os ++ objs (excavar d1) ++ objs (excavar d2)
=                                (HI1, HI2)
  doblar os ++ doblar (objs d1) ++ doblar (objs d2)
=                                (Lema doblar-append)
  doblar os ++ doblar (objs d1 ++ objs d2)
=                                (Lema doblar-append)
  doblar (os ++ objs d1 ++ objs d2)
Lado der:
  doblar (objs (Habitacion os d1 d2))
=                                (objs.2)
  doblar (os ++ objs d1 ++ objs d2)  -- VALE EL CASO VALE LA PROPIEDAD

Lema doblar-append: ¿para todo xs. para todo ys. doblar xs ++ doblar ys = doblar (xs ++ ys)?
Dem: sean zs ws. Por ppio de ind. en la estructura de zs:
CB zs = []) ¿doblar [] ++ doblar ws = doblar ([] ++ ws)?
CI zs = o:os) HI) ¡doblar os ++ doblar ws = doblar (os ++ ws)!
              TI) ¿doblar (o:os) ++ doblar ws = doblar ((o:os) ++ ws)?
Dem CB:
Lado izq:
  doblar [] ++ doblar ws
=                                (doblar.1)
  [] ++ doblar ws
=                                (++.1)
  doblar ws
Lado der:
  doblar ([] ++ ws)
=                                (++.1)
  doblar ws  -- VALE EL CASO
Dem CI:
Lado izq:
  doblar (o:os) ++ doblar ws
=                                (doblar.2)
  (doblarObj o ++ doblar os) ++ doblar ws
=                                (Lema asociatividad-append)
  doblarObj o ++ doblar os ++ doblar ws
=                                (HI)
  doblarObj o ++ doblar (os ++ ws)
Lado der:
  doblar ((o:os) ++ ws)
=                                (++.2)
  doblar (o : os ++ ws)
=                                (doblar.2)
  doblarObj o ++ doblar (os ++ ws)  -- VALE EL CASO VALE LA PROPIEDAD

Lema asociatividad-append: ¿para todo xs. para todo ys. para todo zs. xs ++ (ys ++ zs) = (xs ++ ys) ++ zs?
Dem: sean ws ys' zs'. Por ppio de ind. en la estructura de ws:
C1 ws = []) ¿[] ++ (ys' ++ zs') = ([] ++ ys') ++ zs'?
C2 ws = x:xs') ¿(x:xs') ++ (ys' ++ zs') = ((x:xs') ++ ys') ++ zs'?
Dem C1:
Lado izq:
  [] ++ (ys' ++ zs')
=                                (++.1)
  ys' ++ zs'
Lado der:
  ([] ++ ys') ++ zs'
=                                (++.1)
  ys' ++ zs'  -- VALE EL CASO
Dem C2:
Lado izq:
  (x:xs') ++ (ys' ++ zs')
=                                (++.2)
  x : (xs' ++ ys' ++ zs')
Lado der:
  ((x:xs') ++ ys') ++ zs'
=                                (++.2)
  (x : (xs' ++ ys')) ++ zs'
=                                (++.2)
  x : (xs' ++ ys' ++ zs')  -- VALE EL CASO VALE LA PROPIEDAD

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

Prop: ¿uncompress = subst render heightQT?
Dem: por ppio de ext
¿para todo t. uncompress t = subst render heightQT t?
sea t' un arbol:
Lado izq:
uncompress t'
= def uncompress
expandir (heightQT t') t'
Lado der:
subst render heightQT t'
= def subst
render t' (heightQT t')
= def render
expandir (heightQT t') t'

Prop: ¿para todo t. heightT t >= 0?
Dem: sea t'. Por ppio de ind. en la estructura de t':
CB t' = Empty) ¿heightT Empty >= 0?
CI t' = NodeT x ti td) HI1) ¡heightT t1 >= 0!
                       HI2) ¡heightT t2 >= 0!
                       TI) ¿heightT (NodeT x ti td) >= 0?
Dem CB:
Lado izq:
  heightT Empty
=                                (heightT.1)
  0
Dem CI:
Lado izq:
  heightT (NodeT x ti td)
=                                (heightT.2)
  1 + heightT ti `max` heightT td
=                                (HI1,HI2)
  1 + 0 + 0
=                                (aritm.)
  1
