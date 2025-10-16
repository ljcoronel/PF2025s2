-- Ejercicio 1
data EA = Const Int | BOp BinOp EA EA
data BinOp = Sum | Mul
data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA

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
ea2ExpA (BOp op e1 e2) = operator2expA op (ea2ExpA e1) (ea2ExpA e2)

operator2expA :: BinOp -> ExpA -> ExpA -> ExpA
operator2expA Sum = Suma
operator2expA Mul = Prod

-- Ejercicio iii
expA2ea :: ExpA -> EA
expA2ea (Cte n)      = Const n
expA2ea (Suma e1 e2) = BOp Sum (expA2ea e1) (expA2ea e2)
expA2ea (Prod e1 e2) = BOp Mul (expA2ea e1) (expA2ea e2)

-- Ejercicio 2
data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b)

-- Ejercicio i
cantidadDeHojas :: Arbol a b -> Int
cantidadDeHojas (Hoja _)       = 1
cantidadDeHojas (Nodo _ t1 t2) = cantidadDeHojas t1 + cantidadDeHojas t2

-- Ejercicio ii
cantidadDeNodos :: Arbol a b -> Int
cantidadDeNodos (Hoja _)       = 0
cantidadDeNodos (Nodo _ t1 t2) = 1 + cantidadDeNodos t1 + cantidadDeNodos t2

-- Ejercicio iii
cantidadDeConstructores :: Arbol a b -> Int
cantidadDeConstructores (Hoja _)       = 1
cantidadDeConstructores (Nodo _ t1 t2) = 1 + cantidadDeConstructores t1 + cantidadDeConstructores t2

-- Ejercicio iv
ea2Arbol :: EA -> Arbol BinOp Int
ea2Arbol (Const n)      = Hoja n
ea2Arbol (BOp op e1 e2) = Nodo op (ea2Arbol e1) (ea2Arbol e2)

-- Ejercicio 3
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

-- Ejercicio i
sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

-- Ejercicio ii
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2

-- Ejercicio iii
anyT :: (a -> Bool) -> Tree a -> Bool
anyT _ EmptyT          = False
anyT p (NodeT x t1 t2) = p x || anyT p t1 || anyT p t2

-- Ejercicio iv
countT :: (a -> Bool) -> Tree a -> Int
countT _ EmptyT          = 0
countT p (NodeT x t1 t2) = unoSi (p x) + countT p t1 + countT p t2

unoSi :: Bool -> Int
unoSi True = 1
unoSi _    = 0

-- Ejercicio v
countLeaves :: Tree a -> Int
countLeaves EmptyT          = 1
countLeaves (NodeT _ t1 t2) = countLeaves t1 + countLeaves t2

-- Ejercicio vi
heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT _ t1 t2) = 1 + heightT t1 `max` heightT t2

-- Ejercicio vii
inOrder :: Tree a -> [a]
inOrder EmptyT          = []
inOrder (NodeT x t1 t2) = inOrder t1 ++ [x] ++ inOrder t2

-- Ejercicio viii
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : appendListOfList (listPerLevel t1) (listPerLevel t2)

appendListOfList :: [[a]] -> [[a]] -> [[a]]
appendListOfList []       yss      = yss
appendListOfList xss      []       = xss
appendListOfList (xs:xss) (ys:yss) = (xs ++ ys) : appendListOfList xss yss

-- Ejercicio ix
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)

-- Ejercicio x
--levelN :: Int -> Tree a -> [a]

-- Ejercicio xi
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x t1 t2) = x : listaMasLarga (ramaMasLarga t1) (ramaMasLarga t2)

listaMasLarga :: [a] -> [a] -> [a]
listaMasLarga xs ys = if length xs > length ys
                       then xs
                       else ys

-- Ejercicio xii
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x t1 t2) = agregarElemento x (todosLosCaminos t1 ++ todosLosCaminos t2)

agregarElemento :: a -> [[a]] -> [[a]]
agregarElemento x []       = [x] : []
agregarElemento x (xs:xss) = (x : xs) : agregarElemento x xss

-- Ejercicio 4
data AppList a = Single a | Append (AppList a) (AppList a)

-- Ejercicio i
lenAL :: AppList a -> Int
lenAL (Single _)     = 1
lenAL (Append t1 t2) = lenAL t1 + lenAL t2

-- Ejercicio ii
consAL :: a -> AppList a -> AppList a
consAL x (Single y)     = Append (Single x) (Single y)
consAL x (Append t1 t2) = Append (consAL x t1) t2

-- Ejercicio iii
headAL :: AppList a -> a
headAL (Single x)    = x
headAL (Append t1 _) = headAL t1

-- Ejercicio iv
tailAL :: AppList a -> AppList a
-- PRECOND: La lista tiene mas de un elemento.
tailAL (Single x)             = error "La lista no puede quedar vacia"
tailAL (Append (Single _) t2) = t2
tailAL (Append t1         t2) = Append (tailAL t1) t2

-- Ejercicio v
snocAL :: AppList a -> a -> AppList a
snocAL (Single y)     x = Append (Single y) (Single x)
snocAL (Append t1 t2) x = Append t1 (snocAL t2 x)

-- Ejercicio vi
lastAL :: AppList a -> a
lastAL (Single x)     = x
lastAL (Append t1 t2) = lastAL t2

-- Ejercicio vii
initAL :: AppList a -> AppList a
-- PRECOND: La lista tiene mas de un elemento.
initAL (Single _)             = error "La lista no puede quedar vacia"
initAL (Append t1 (Single _)) = t1
initAL (Append t1 t2)         = Append t1 (initAL t2)

-- Ejercicio viii
reverseAL :: AppList a -> AppList a
reverseAL (Single x)     = Single x
reverseAL (Append t1 t2) = Append (reverseAL t2) (reverseAL t1)

-- Ejercicio ix
elemAL :: Eq a => a -> AppList a -> Bool
elemAL x (Single y)     = x==y
elemAL x (Append t1 t2) = elemAL x t1 || elemAL x t2

-- Ejercicio x
appendAL :: AppList a -> AppList a -> AppList a
appendAL list (Single x)     = Append list (Single x)
appendAL list (Append t1 t2) = Append (appendAL list t1) t2

-- Ejercicio xi
appListToList :: AppList a -> [a]
appListToList (Single x)     = [x]
appListToList (Append t1 t2) = appListToList t1 ++ appListToList t2

-- Ejercicio 5
data QuadTree a = LeafQ a | NodeQ (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)
data Color = RGB Int Int Int
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
uncompress (LeafQ x)           = NodeQ (LeafQ x) (LeafQ x) (LeafQ x) (LeafQ x)
uncompress (NodeQ t1 t2 t3 t4) = let (h1, h2, h3, h4) = (heightQT t1, heightQT t2, heightQT t3, heightQT t4)
                                  in if h1 == h2 && h2 == h3 && h3 == h4
                                      then NodeQ t1 t2 t3 t4
                                      else if h1 < h2 || h1 < h3 || h1 < h4
                                            then NodeQ (uncompress t1) t2 t3 t4
                                            else if h2 < h1 || h2 < h3 || h2 < h4
                                                  then NodeQ t1 (uncompress t2) t3 t4
                                                  else if h3 < h1 || h3 < h2 || h3 < h4
                                                        then NodeQ t1 t2 (uncompress t3) t4
                                                        else NodeQ t1 t2 t3 (uncompress t4)

-- Ejercicio vi
--render :: Image -> Int -> Image