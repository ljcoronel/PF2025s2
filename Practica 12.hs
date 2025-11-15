data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA
     deriving Show

--Cte :: Int -> ExpA
--c :: Int -> b
--Suma :: ExpA -> ExpA -> ExpA
--s :: b -> b -> b
--Prod :: ExpA -> ExpA -> ExpA
--p :: b -> b -> b
--Var :: String -> ExpA
--v :: String -> b

-- Ejemplos
expa1 = Cte 0
expa2 = Cte 1
expa3 = Suma (Cte 0) (Cte 5)
expa4 = Prod (Suma (Cte 2) (Cte 3)) (Cte 4)
expa5 = Suma (Prod (Cte 0) (Cte 7)) (Suma (Cte 3) (Cte 0))
expa6 = Suma (Suma (Cte 0) (Cte 7)) (Suma (Cte 3) (Cte 0))
expa7 = Cte (-1)
expa8 = Prod (Suma (Cte (-5)) (Cte (-7))) (Prod (Cte (1)) (Cte (-3)))
expa9 = Prod (Cte 1) (Cte 6)
expa10 = Suma (Cte 0) (Cte 0)
expa11 = Prod (Cte 1) (Cte 1)

-- Ejercicio a
foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA c _ _ (Cte n)      = c n
foldExpA c s p (Suma e1 e2) = s (foldExpA c s p e1) (foldExpA c s p e2)
foldExpA c s p (Prod e1 e2) = p (foldExpA c s p e1) (foldExpA c s p e2)

-- Ejercicio b
-- Ejercicio i
cantidadDeCeros :: ExpA -> Int
cantidadDeCeros = foldExpA unoSiEsCero (+) (+)

unoSiEsCero :: Int -> Int
unoSiEsCero 0 = 1
unoSiEsCero _ = 0

-- Ejercicio ii
noTieneNegativosExplicitosExpA :: ExpA -> Bool
noTieneNegativosExplicitosExpA = foldExpA (>=0) (&&) (&&)

-- Ejercicio iii
simplificarExpA' :: ExpA -> ExpA
simplificarExpA' = foldExpA Cte simplificarSumaExpA' simplificarProdExpA'

simplificarSumaExpA' :: ExpA -> ExpA -> ExpA
simplificarSumaExpA' (Cte 0) e       = e
simplificarSumaExpA' e       (Cte 0) = e
simplificarSumaExpA' e1      e2      = Suma e1 e2

simplificarProdExpA' :: ExpA -> ExpA -> ExpA
simplificarProdExpA' (Cte 1) e       = e
simplificarProdExpA' e       (Cte 1) = e
simplificarProdExpA' (Cte 0) _       = Cte 0
simplificarProdExpA' _       (Cte 0) = Cte 0
simplificarProdExpA' e1      e2      = Prod e1 e2

-- Ejercicio iv
evalExpA' :: ExpA -> Int
evalExpA' = foldExpA id (+) (*)

-- Ejercicio v
showExpA :: ExpA -> String
showExpA = foldExpA (\n -> "(Cte" ++ show n ++ ")") (\s1 s2 -> "(Suma" ++ s1 ++ s2 ++ ")") ((\s1 s2 -> "(Prod" ++ s1 ++ s2 ++ ")"))

-- Ejercicio d
recExpA :: (Int -> b) -> (ExpA -> ExpA -> b -> b -> b) -> (ExpA -> ExpA -> b -> b -> b) -> ExpA -> b
recExpA c _ _ (Cte n)      = c n
recExpA c s p (Suma e1 e2) = s e1 e2 (recExpA c s p e1) (recExpA c s p e2)
recExpA c s p (Prod e1 e2) = p e1 e2 (recExpA c s p e1) (recExpA c s p e2)

-- Ejercicio e
-- Ejercicio i
cantDeSumaCeros :: ExpA -> Int
cantDeSumaCeros = recExpA (const 0) (\e1 e2 -> ((+) . ((+) (unoSiSumaCero e1 e2)))) (const . const (+))

unoSiSumaCero :: ExpA -> ExpA -> Int
unoSiSumaCero (Cte 0) _       = 1
unoSiSumaCero _       (Cte 0) = 1
unoSiSumaCero _       _       = 0

-- Ejercicio ii
cantDeProdUnos :: ExpA -> Int
cantDeProdUnos = recExpA (const 0) (const . const (+)) (\e1 e2 -> ((+) . ((+) (unoSiProdUno e1 e2))))

unoSiProdUno :: ExpA -> ExpA -> Int
unoSiProdUno (Cte 1) _       = 1
unoSiProdUno _       (Cte 1) = 1
unoSiProdUno _       _       = 0

-- Ejercicio 2
data EA = Const Int | BOp BinOp EA EA
     deriving Show
data BinOp = Sum | Mul
     deriving Show

--Const :: Int -> EA
--c :: Int -> b
--BOp :: BinOp -> EA -> EA -> EA
--b :: BinOp -> b -> b -> b

-- Ejemplos
ea1 = Const 0
ea2 = Const 1
ea3 = BOp Sum (Const 0) (Const 2)
ea4 = BOp Sum (Const 1) (Const 0)
ea5 = BOp Sum (Const 0) (Const 0)
ea6 = BOp Mul (Const 1) (Const 3)
ea7 = BOp Mul (Const 2) (Const 1)
ea8 = BOp Mul (Const 0) (Const 2)
ea9 = BOp Mul (Const 2) (Const 0)
ea10 = BOp Mul (Const 1) (Const 0)
ea11 = BOp Mul (Const 0) (Const 1)
ea12 = Const (-1)
ea13 = BOp Sum (Const 0) (Const (-2))
ea14 = BOp Mul (Const 0) (Const (-1))
ea15 = BOp Sum (BOp Mul (Const 7) (Const 2)) (Const 6)

-- Ejercicio a
foldEA :: (Int -> b) -> (BinOp -> b -> b -> b) -> EA -> b
foldEA c _ (Const n)      = c n
foldEA c b (BOp op e1 e2) = b op (foldEA c b e1) (foldEA c b e2)

-- Ejercicio b
data ABTree a b = Leaf b | Node a (ABTree a b) (ABTree a b)
     deriving Show

-- Ejercicio i
noTieneNegativosExplicitosEA :: EA -> Bool
noTieneNegativosExplicitosEA = foldEA (>=0) (const (&&))

-- Ejercicio ii
simplificarEA' :: EA -> EA
simplificarEA' = foldEA Const simplificarBinOp

simplificarBinOp :: BinOp -> EA -> EA -> EA
simplificarBinOp Sum = simplificarSumEA
simplificarBinOp Mul = simplificarMulEA

simplificarSumEA :: EA -> EA -> EA
simplificarSumEA (Const 0) e         = e
simplificarSumEA e         (Const 0) = e
simplificarSumEA e1        e2        = BOp Sum e1 e2

simplificarMulEA :: EA -> EA -> EA
simplificarMulEA (Const 1) e         = e
simplificarMulEA e         (Const 1) = e
simplificarMulEA (Const 0) _         = Const 0
simplificarMulEA _         (Const 0) = Const 0
simplificarMulEA e1        e2        = BOp Mul e1 e2

-- Ejercicio iii
evalEA' :: EA -> Int
evalEA' = foldEA id evalBinOp

evalBinOp :: BinOp -> Int -> Int -> Int
evalBinOp Sum = (+)
evalBinOp Mul = (*)

-- Ejercicio iv
showEA :: EA -> String
showEA = foldEA (\n -> "(Const" ++ show n ++ ")") (\op s1 s2 -> "(BOp" ++ show op ++ s1 ++ s2 ++ ")")

-- Ejercicio v
ea2ExpA' :: EA -> ExpA
ea2ExpA' = foldEA Cte binOp2ExpA

binOp2ExpA :: BinOp -> ExpA -> ExpA -> ExpA
binOp2ExpA Sum = Suma
binOp2ExpA Mul = Prod

-- Ejercicio vi
ea2Arbol' :: EA -> ABTree BinOp Int
ea2Arbol' = foldEA Leaf Node

-- Ejercicio 3
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
     deriving Show

--EmptyT :: Tree a
--z :: b
--NodeT :: a -> Tree a -> Tree a -> Tree a
--f :: a -> b -> b -> b

--EJEMPLOS
tree1 :: Tree Int
tree1 = NodeT 1 (NodeT 2 (NodeT 3 EmptyT
                                  EmptyT)
                          EmptyT)
                (NodeT 4 (NodeT 5 EmptyT
                                  EmptyT)
                          EmptyT)
tree2 :: Tree Int
tree2 = NodeT 1 (NodeT 2 (NodeT 4 EmptyT
                                  EmptyT)
                         (NodeT 5 EmptyT
                                  EmptyT))
                (NodeT 3 (NodeT 6 EmptyT
                                  EmptyT)
                         (NodeT 7 (NodeT 8 EmptyT
                                           EmptyT)
                                  (NodeT 9 EmptyT
                                           EmptyT)))

tree3 :: Tree Int
tree3 = NodeT 1 (NodeT 2 (NodeT 4 EmptyT
                                  EmptyT)
                         (NodeT 5 EmptyT
                                  EmptyT))
                (NodeT 2 (NodeT 6 EmptyT
                                  EmptyT)
                         (NodeT 7 (NodeT 8 EmptyT
                                           EmptyT)
                                  (NodeT 9 EmptyT
                                           EmptyT)))

-- Ejercicio a
foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
foldT z _ EmptyT          = z
foldT z f (NodeT x ti td) = f x (foldT z f ti) (foldT z f td)

-- Ejercicio b
-- Ejercicio i
mapT :: (a -> b) -> Tree a -> Tree b
mapT f = foldT EmptyT (NodeT . f)

-- Ejercicio ii
sumT :: Tree Int -> Int
sumT = foldT 0 (\n -> (+) . (n+))

-- Ejercicio iii
sizeT :: Tree a -> Int
sizeT = foldT 0 (const ((+) . (1+)))

-- Ejercicio iv
heightT :: Tree a -> Int
heightT = foldT 0 (\_ ti td -> 1 + ti `max` td)

-- Ejercicio v
preOrder :: Tree a -> [a]
preOrder = foldT [] (\e xs ys -> [e] ++ xs ++ ys)

-- Ejercicio vi
inOrder :: Tree a -> [a]
inOrder = foldT [] (\e xs ys -> xs ++ [e] ++ ys)

-- Ejercicio vii
postOrder :: Tree a -> [a]
postOrder = foldT [] (\e xs ys -> xs ++ ys ++ [e])

-- Ejercicio viii
mirrorT :: Tree a -> Tree a
mirrorT = foldT EmptyT (\x ti td -> NodeT x td ti)

-- Ejercicio ix
countByT :: (a -> Bool) -> Tree a -> Int
countByT p = foldT 0 (\x n1 n2 -> unoSi (p x) + n1 + n2)

unoSi :: Bool -> Int
unoSi True = 1
unoSi _    = 0

-- Ejercicio x
partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
partitionT p = foldT ([],[]) (\e (xs,ys) (xs',ys') -> let (zs,ws) = (xs++xs',ys++ys') in if p e then (e:zs,ws) else (zs,e:ws))

-- Ejercicio xi
zipWithT :: (a -> b -> c) -> Tree a -> Tree b -> Tree c  -- ATENCION A LOS TIPOS EL CASO BASE NO PUEDE SER EmptyT b :: Tree b -> Tree c!!
zipWithT f = foldT (const EmptyT) (\x g h -> \t -> case t of EmptyT -> EmptyT; NodeT y ti td -> NodeT (f x y) (g ti) (h td))

-- Ejercicio xii
caminoMasLargo :: Tree a -> [a] -- NO OLVIDAR EL \ DEL LAMBDA Y length devuelve un numero!!!!!
caminoMasLargo = foldT [] (\e xs ys -> e : if length xs > length ys then xs else ys)

-- Ejercicio xiii (CONSULTAR RECURSION)
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos = foldT [] (\e xss yss -> [e] : agregarRaiz e (xss ++ yss))

agregarRaiz :: a -> [[a]] -> [[a]]
agregarRaiz x = map (x:)

-- Ejercicio xiv
todosLosNiveles :: Tree a -> [[a]]
todosLosNiveles = foldT [] (\e xss yss -> [e] : concatPerLevel xss yss)

concatPerLevel :: [[a]] -> [[a]] -> [[a]]
concatPerLevel = foldr (\xs h -> \yss -> case yss of [] -> xs : h yss; (ys:yss) -> (xs ++ ys) : h yss) id

-- Ejercicio xv
nivelN :: Tree a -> Int -> [a]
nivelN = foldT (const []) (\x g h -> \n -> case n of 0 -> [x]; n -> g (n-1) ++ h (n-1))

-- Ejercicio c
recT :: b -> (a -> Tree a -> Tree a -> b -> b -> b) -> Tree a -> b
recT z _ EmptyT          = z
recT z f (NodeT x ti td) = f x ti td (recT z f ti) (recT z f td)

-- Ejercicio d
-- Ejercicio i
insertT :: Ord a => a -> Tree a -> Tree a
insertT x = recT (NodeT x EmptyT EmptyT) (\y ti td ri rd -> if x==y then NodeT y ti td else if x<y then NodeT y ri td else NodeT y ti rd)

-- Ejercicio ii
caminoHasta :: Eq a => a -> Tree a -> [a]
caminoHasta x t = case mCaminoHasta x t of
                   Nothing -> error "No existe el elemento dado en el arbol dado"
                   Just xs -> xs

mCaminoHasta :: Eq a => a -> Tree a -> Maybe [a]
mCaminoHasta x = foldT Nothing (\y mi md -> if x==y then Just [] else agregarCamino y mi md)

agregarCamino :: a -> Maybe [a] -> Maybe [a] -> Maybe [a]
agregarCamino x (Just xs) _         = Just (x:xs)
agregarCamino x _         (Just xs) = Just (x:xs)
agregarCamino _ _         _         = Nothing

-- Ejercicio 4
type Record a b = [(a,b)]
type Table a b = [ Record a b ]

-- Ejercicio a
select :: (Record a b -> Bool) -> Table a b -> Table a b
select = filter

-- Ejercicio b
project :: (a -> Bool) -> Table a b -> Table a b
project p = map (filter (p . fst))

-- Ejercicio c
conjunct :: (a -> Bool) -> (a -> Bool) -> a -> Bool
conjunct p1 p2 x = p1 x && p2 x

-- Ejercicio d
crossWith :: (a -> b -> c) -> [a] -> [b] -> [c]
crossWith f = foldr (\x h -> \ys -> map (f x) ys ++ h ys) (const [])

-- Ejercicio e
product' :: Table a b -> Table a b -> Table a b
product' = crossWith (++)
--product = foldr (\rs h -> \t -> map (rs ++) t ++ h t) (const [])

-- Ejercicio f
similar :: (Eq b, Eq a) => Record a b -> Record a b
similar = foldr (\d ds -> if elem d ds then ds else d : ds) []

-- Ejercicio 5
data Query a b = Table (Table a b) | Product (Query a b) (Query a b) | Projection (a -> Bool) (Query a b) | Selection (Record a b -> Bool) (Query a b)

--Table :: Table a b -> Query a b
--t :: Table a b -> c
--Product :: Query a b -> Query a b -> Query a b
--pd :: c -> c -> c
--Projection :: (a -> Bool) -> Query a b -> Query a b
--pj :: (a -> Bool) -> c -> c
--Selection :: (Record a b -> Bool) -> Query a b -> Query a b
--s :: (Record a b -> Bool) -> c -> c

-- Ejercicio a
foldQ :: (Table a b -> c) -> (c -> c -> c) -> ((a -> Bool) -> c -> c) -> ((Record a b -> Bool) -> c -> c) -> Query a b -> c
foldQ t pd pj s (Table rs)       = t rs
foldQ t pd pj s (Product q1 q2)  = pd (foldQ t pd pj s q1) (foldQ t pd pj s q2)
foldQ t pd pj s (Projection p q) = pj p (foldQ t pd pj s q)
foldQ t pd pj s (Selection p q)  = s p (foldQ t pd pj s q)
-- ERRORES COMETIDOS AL HACER foldQ
--en el caso 1 me confundi y puse s en vez de t NO CONFUNDIR EL ORDEN, SEGURAMENTE ME CONFUNDO POR foldr
--en el caso 3 confundi el tipo Bool por b
--en uno de los casos me olvide un argumento

recQ :: (Table a b -> c)
     -> (Query a b -> Query a b -> c -> c -> c)
     -> ((a -> Bool) -> Query a b -> c -> c)
     -> ((Record a b -> Bool) -> Query a b -> c -> c)
     -> Query a b
     -> c
recQ t pd pj s (Table rs)       = t rs
recQ t pd pj s (Product q1 q2)  = pd q1 q2 (recQ t pd pj s q1) (recQ t pd pj s q2)
recQ t pd pj s (Projection p q) = pj p q (recQ t pd pj s q)
recQ t pd pj s (Selection p q)  = s p q (recQ t pd pj s q)

-- Ejercicio i
tables :: Query a b -> [Table a b]
tables = foldQ (flip (:) []) (++) (const id) (const id)
             --(\rs -> [rs]) --(\rs1 rs2 -> rs1 ++ rs2)

-- Ejercicio ii
execute :: Query a b -> Table a b
execute = foldQ id product' project select

-- Ejercicio iii
compact :: Query a b -> Query a b
compact = foldQ Table Product compactProjection compactSelection

compactProjection :: (a -> Bool) -> Query a b -> Query a b
compactProjection p1 (Projection p2 q) = Projection (conjunct p1 p2) q
compactProjection p  q                 = Projection p q

compactSelection :: (Record a b -> Bool) -> Query a b -> Query a b
compactSelection p1 (Selection p2 q) = Selection (conjunct p1 p2) q
compactSelection p  q                = Selection p q

-- Ejercicio 6
data Dir = LeftD | RightD | Straight
     deriving Show
data Mapa a = Cofre [a] | Nada (Mapa a) | Bifurcacion [a] (Mapa a) (Mapa a)
     deriving Show

-- Ejercicio a
foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM c n b (Cofre os)             = c os
foldM c n b (Nada m)               = n (foldM c n b m)
foldM c n b (Bifurcacion os mi md) = b os (foldM c n b mi) (foldM c n b md)

recM :: ([a] -> b) -> (Mapa a -> b -> b) -> ([a] -> Mapa a -> Mapa a -> b -> b -> b) -> Mapa a -> b
recM c n b (Cofre os)             = c os
recM c n b (Nada m)               = n m (recM c n b m)
recM c n b (Bifurcacion os mi md) = b os mi md (recM c n b mi) (recM c n b md)

-- Ejercicio b
-- Ejercicio i
objects :: Mapa a -> [a]
objects = foldM id id (\xs os1 os2 -> xs ++ os1 ++ os2)

-- Ejercicio ii
mapM :: (a -> b) -> Mapa a -> Mapa b
mapM f = foldM (Cofre . map f) Nada (Bifurcacion . map f)

-- Ejercicio iii
has :: (a -> Bool) -> Mapa a -> Bool
has p = foldM (any p) id (\xs b1 b2 -> any p xs || b1 || b2)

-- Ejercicio iv
hasObjectAt :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt p = foldM (hasObjectAtC p) hasObjectAtN (hasObjectAtB p)

hasObjectAtC :: (a -> Bool) -> [a] -> [Dir] -> Bool
hasObjectAtC p os [] = any p os
hasObjectAtC _ _  _  = False

hasObjectAtN :: ([Dir] -> Bool) -> [Dir] -> Bool
hasObjectAtN f (Straight:ds) = f ds
hasObjectAtN _ _             = False

hasObjectAtB :: (a -> Bool) -> [a] -> ([Dir] -> Bool) -> ([Dir] -> Bool) -> [Dir] -> Bool
hasObjectAtB p os _ _ []          = any p os
hasObjectAtB _ _  f _ (LeftD:ds)  = f ds
hasObjectAtB _ _  _ f (RightD:ds) = f ds
hasObjectAtB _ _  _ _ _           = False

-- Ejercicio v
longestPath :: Mapa a -> [Dir]
longestPath = foldM (const [])
                    (Straight :)
                    (\_ ds1 ds2 -> if length ds1 > length ds2 then LeftD : ds1 else RightD : ds2)

-- Ejercicio vi
objectsOfLongestPath :: Mapa a -> [a]
objectsOfLongestPath = snd . levelAndObjectsOfLongestPath

levelAndObjectsOfLongestPath :: Mapa a -> (Int,[a])
levelAndObjectsOfLongestPath = foldM (\os -> (0,os))
                                     (\(n,os) -> (n+1,os))
                                     (\os (n,os1) (m,os2) -> if n > m then (n+1,os++os1) else (m+1,os++os2))

listOfObjectOfLongestPath :: Mapa a -> [[a]]
listOfObjectOfLongestPath = foldM (flip (:) [])
                                  id
                                  (\os oss1 oss2 -> os : if length oss1 > length oss2 then oss1 else oss2)

-- Ejercicio vii
allPaths :: Mapa a -> [[Dir]]
allPaths = foldM (const [])
                 (([Straight] :) . agregarRaiz Straight)
                 (\_ dss1 dss2 -> ([LeftD] : agregarRaiz LeftD dss1) ++ ([RightD] : agregarRaiz RightD dss2))

-- Ejercicio viii
objectsPerLevel :: Mapa a -> [[a]]
objectsPerLevel = foldM (flip (:) []) ((:) []) (\os oss1 oss2 -> os : concatPerLevel oss1 oss2)

-- Ejercicio 7
data GTree a = GNode a [GTree a]
     deriving Show

--Ejemplo
gt :: GTree Int
gt = GNode 1 [ GNode 2 [ GNode 5 []
                       , GNode 6 [] ]
             , GNode 3 [ GNode 7 [] ]
             , GNode 4 [] ]
gt0 :: GTree Int
gt0 = GNode 10 []
mgt :: GTree Int
mgt = GNode 1 [GNode 4 [],GNode 3 [GNode 7 []],GNode 2 [GNode 6 [],GNode 5 []]]
-- preOrderGT gt = [1,2,5,6,3,7,4]
-- postOrderGT gt = [5,6,2,7,3,4,1]
-- mirrorGT gt = GNode 1 [ GNode 4 []
--                       , GNode 3 [ GNode 7 [] ]
--                       , GNode 2 [ GNode 6 []
--                                 , GNode 5 [] ] ]
-- todosLosCaminos gt = [[1],[1,2],[1,2,5],[1,2,6],[1,3],[1,3,7],[1,4]]
-- todosLosCaminos gt0 = [[10]]
-- todosLosNiveles gt = [[1],[2,3,4],[5,6,7]]
-- todosLosNiveles gt0 = [[10]]

-- Ejercicio b (con recursion explicita)
-- Ejercicio i
mapGT :: (a -> b) -> GTree a -> GTree b
mapGT f (GNode x ts) = GNode (f x) (map (mapGT f) ts)

-- Ejercicio ii
sumGT :: GTree Int -> Int
sumGT (GNode n ts) = n + sum (map sumGT ts)

-- Ejercicio iii
sizeGT :: GTree a -> Int
sizeGT (GNode x ts) = 1 + sum (map sizeGT ts)

-- Ejercicio iv
heightGT :: GTree a -> Int
heightGT (GNode x ts) = 1 + maxOr 0 (map heightGT ts)

maxOr :: Int -> [Int] -> Int
maxOr n [] = n
maxOr _ xs = maximum xs

-- Ejercicio v
preOrderGT :: GTree a -> [a]
preOrderGT (GNode x ts) = [x] ++ concat (map preOrderGT ts)

-- Ejercicio vi
postOrderGT :: GTree a -> [a]
postOrderGT (GNode x ts) = concat (map postOrderGT ts) ++ [x]

-- Ejercicio vii
mirrorGT :: GTree a -> GTree a
mirrorGT (GNode x ts) = GNode x (reverse (map mirrorGT ts))

-- Ejercicio viii
countByGT :: (a -> Bool) -> GTree a -> Int
countByGT p (GNode x ts) = unoSi (p x) + sum (map (countByGT p) ts)

-- Ejercicio ix
partitionGT :: (a -> Bool) -> GTree a -> ([a], [a])
partitionGT p (GNode x ts) = let (ri,rd) = concatPair (map (partitionGT p) ts)
                              in if p x
                                  then (x:ri,rd)
                                  else (ri,x:rd)

concatPair :: [([a],[a])] -> ([a],[a])
concatPair []      = ([],[])
concatPair (ps:xs) = (fst ps ++ fst (concatPair xs), snd ps ++ snd (concatPair xs))

-- Ejercicio x
zipWithGT :: (a -> b -> c) -> GTree a -> GTree b -> GTree c
zipWithGT f (GNode x ts1) (GNode y ts2) = let rs = foldr (\t h -> \ts -> case ts of [] -> []; y:ys -> zipWithGT f t y : h ys) (const []) ts1 ts2
                                           in GNode (f x y) rs

-- Ejercicio xi
caminoMasLargoGT :: GTree a -> [a]
caminoMasLargoGT (GNode x ts) = x : laMasLarga (map caminoMasLargoGT ts)

laMasLarga :: [[a]] -> [a]
laMasLarga []       = []
laMasLarga (xs:xss) = if length xs > length (laMasLarga xss)
                       then xs
                       else laMasLarga xss

-- Ejercicio xii
todosLosCaminosGT :: GTree a -> [[a]]
todosLosCaminosGT (GNode x ts) = [x] : agregarRaiz x (concat (map todosLosCaminosGT ts))

-- Ejercicio xiii
todosLosNivelesGT :: GTree a -> [[a]]
todosLosNivelesGT (GNode x ts) = [x] : concatPerLevelGT (map todosLosNivelesGT ts)

concatPerLevelGT :: [[[a]]] -> [[a]]
concatPerLevelGT []         = []
concatPerLevelGT (xss:xsss) = concatPerLevel xss (concatPerLevelGT xsss)

-- Ejercicio xiv
caminoHastaGT :: Eq a => a -> GTree a -> [a]
caminoHastaGT e t = case mCaminoHastaGT e t of
                     Nothing -> error "No existe el elemento"
                     Just xs -> xs

mCaminoHastaGT :: Eq a => a -> GTree a -> Maybe [a]
mCaminoHastaGT e (GNode x ts) = if e==x
                                 then Just []
                                 else agregarM x (findM (map (mCaminoHastaGT e) ts))

agregarM :: a -> Maybe [a] -> Maybe [a]
agregarM x (Just xs) = Just (x:xs)
agregarM _ _         = Nothing

findM :: [Maybe a] -> Maybe a
findM []           = Nothing
findM (Nothing:ms) = findM ms
findM (m:_)        = m

-- Ejercicio xv
nivelNGT :: GTree a -> Int -> [a]
nivelNGT (GNode x ts) 0 = [x]
nivelNGT (GNode x ts) n = foldr (\t -> (++) (nivelNGT t (n-1))) [] ts

-- Ejercicio 8
type Name = String
type Content = String
type Path = [Name]
data FileSystem = File Name Content | Folder Name [FileSystem]
     deriving Show

-- Ejemplos:
fs0 = File "img" "Foto"
fs1 = File "mp3" "Musica"
fs2 = File "mp4" "Video"

fs3 = Folder "New" []
fs4 = Folder "Misc" [fs0, fs1]
fs5 = Folder "files" [fs3, fs4, fs2, fs1]
-- Ejercicio b (con recursion explicita)
-- Ejericicio i
amountOfFiles :: FileSystem -> Int
amountOfFiles (File _ _)    = 1
amountOfFiles (Folder _ fs) = sum (map amountOfFiles fs)

-- Ejericicio ii
find :: Name -> FileSystem -> Maybe Content
find n (File n' c)   = if n==n' then Just c else Nothing
find n (Folder _ fs) = findM (map (find n) fs)

-- Ejericicio iii
pathOf :: Name -> FileSystem -> Path
-- PRECOND: El nombre existe en el filesystem dado
pathOf n fs = case pathOfM n fs of
               Nothing -> error "no existe el nombre"
               Just p  -> p

pathOfM :: Name -> FileSystem -> Maybe Path
pathOfM n (File n' _)    = if n==n' then Just [] else Nothing
pathOfM n (Folder n' fs) = agregarM n' (findM (map (pathOfM n) fs))

-- Ejericicio iv
mapContents :: (Content -> Content) -> FileSystem -> FileSystem
mapContents f (File n c)    = File n (f c)
mapContents f (Folder n fs) = Folder n (map (mapContents f) fs)

-- Ejericicio v
targetedMapContents :: [(Name, Content -> Content)] -> FileSystem -> FileSystem
targetedMapContents pfs (File n c)    = File n (searchFS pfs n c)
targetedMapContents pfs (Folder n fs) = Folder n (map (targetedMapContents pfs) fs)

searchFS :: [(Name, Content -> Content)] -> Name -> Content -> Content
searchFS []           _ = id
searchFS ((n',f):pfs) n = if n'==n then f else searchFS pfs n

-- Funciones explicitas para demos
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT _ ti td) = 1 + sizeT ti + sizeT td

mapT :: (a -> b) -> Tree a -> Tree b
mapT _ EmptyT          = EmptyT
mapT f (NodeT x ti td) = NodeT (f x) (mapT f ti) (mapT f td)

has :: (a -> Bool) -> Mapa a -> Bool
has p (Cofre os)             = any p os
has p (Nada m)               = has p m
has p (Bifurcacion os m1 m2) = any p os || has p m1 || has p m2

objectsPerLevel :: Mapa a -> [[a]]
objectsPerLevel (Cofre os)             = [os]
objectsPerLevel (Nada m)               = [] : objectsPerLevel m
objectsPerLevel (Bifurcacion os m1 m2) = os : concatPerLevel (objectsPerLevel m1) (objectsPerLevel m2)

preOrder :: Tree a -> [a]
preOrder EmptyT          = []
preOrder (NodeT x ti td) = [x] ++ preOrder ti ++ preOrder td

execute :: Query a b -> Table a b
execute (Table rs)       = rs
execute (Product q1 q2)  = product (execute q1) (execute q2)
execute (Projection p q) = project p (execute q)
execute (Selection p q)  = select p (execute q)

product :: Table a b -> Table a b -> Table a b
product []     _ = []
product (r:rs) t = (productT t r) ++ product rs t

productT :: Table a b -> Record a b -> Table a b
productT []     _  = []
productT (r:rs) r' = (r ++ r') : productT rs r'

project :: (a -> Bool) -> Table a b -> Table a b
project _ []     = []
project p (r:rs) = projectR p r : project p rs

projectR :: (a -> Bool) -> Record a b -> Record a b
projectR _ []     = []
projectR p (d:ds) = if p (fst d) then d : projectR p ds else projectR p ds

select :: (Record a b -> Bool) -> Table a b -> Table a b
select p []     = []
select p (r:rs) = if p r then r : select p rs else select p rs

compact :: Query a b -> Query a b
compact (Table rs)       = Table rs
compact (Product q1 q2)  = Product (compact q1) (compact q2)
compact (Projection p q) = compactProjection p (compact q)
compact (Selection p q   = compactSelection p (compact q)

heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT x ti td) = 1 + heightT ti `max` heightT td

zipWithT :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipWithT _ EmptyT            _                 = EmptyT
zipWithT _ _                 EmptyT            = EmptyT
zipWithT f (NodeT x ti1 td1) (NodeT y ti2 td2) = NodeT (f x y) (zipWithT f ti1 ti2) (zipWithT f td1 td2)

DEMOS
-- Ejercicio 1 C
Prop: ¿evalExpA' = evalExpA?
Dem: por ppio de ext. y por evalExpA'
¿para todo e. foldExpA id (+) (*) e = evalExpA e?
sea e'. Por ppio de ind. en la estructura de e':
CB e' = Cte n) ¿foldExpA id (+) (*) (Cte n) = evalExpA (Cte n)?
CI.1 e' = Suma e1 e2) HI11) ¡foldExpA id (+) (*) e1 = evalExpA e1!
                      HI21) ¡foldExpA id (+) (*) e2 = evalExpA e2!
                      TI1) ¿foldExpA id (+) (*) (Suma e1 e2) = evalExpA (Suma e1 e2)?
CI.2 e' = Prod e1 e2) HI12) ¡foldExpA id (+) (*) e1 = evalExpA e1!
                      HI22) ¡foldExpA id (+) (*) e2 = evalExpA e2!
                      TI2) ¿foldExpA id (+) (*) (Prod e1 e2) = evalExpA (Prod e1 e2)?
Dem CB:
Lado izq:
foldExpA id (+) (*) (Cte n)
= def foldExpA.1
id n
= def id
n
Lado der:
evalExpA (Cte n)
= def evalExpA.1
n  -- VALE EL CASO
Dem CI.1:
Lado izq:
foldExpA id (+) (*) (Suma e1 e2)
= def foldExpA.2
(+) (foldExpA id (+) (*) e1) (foldExpA id (+) (*) e2)
= seccion de operadores, HI11, HI22
evalExpA e1 + evalExpA e2
Lado der:
evalExpA (Suma e1 e2)
= def evalExpA.2
evalExpA e1 + evalExpA e2  -- VALE EL CASO
Dem CI.2:
Lado izq:
foldExpA id (+) (*) (Prod e1 e2)
= def foldExpA.3
(*) (foldExpA id (+) (*) e1) (foldExpA id (+) (*) e2)
= seccion de operadores, HI12, HI22
evalExpA e1 * evalExpA e2
Lado der:
evalExpA (Prod e1 e2)
= def evalExpA.3
evalExpA e1 * evalExpA e2  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio 2 C
Prop: ¿evalEA' = evalEA?
Dem: por ppio de ext. y por evalEA'
¿para todo e. foldEA id evalBinOp e = evalEA e?
sea e'. Por ppio de ind. en la estructura de e':
CB e' = Const n) ¿foldEA id evalBinOp (Const n) = evalEA (Const n)?
CI e' = BOp op e1 e2) HI1) ¡foldEA id evalBinOp e1 = evalEA e1!
                      HI2) ¡foldEA id evalBinOp e2 = evalEA e2!
                      TI) ¿foldEA id evalBinOp (BOp op e1 e2) = evalEA (BOp op e1 e2)?
Dem CB:
Lado izq:
foldEA id evalBinOp (Const n)
= def foldEA.1
id n
= def id
n
Lado der:
evalEA (Const n)
= def evalEA.1
n  -- VALE EL CASO
Dem CI:
Lado izq:
foldEA id evalBinOp (BOp op e1 e2)
= def foldEA.2
evalBinOp op (foldEA id evalBinOp e1) (foldEA id evalBinOp e2)
= HI1, HI2
evalBinOp op (evalEA e1) (evalEA e2)
Lado der:
evalEA (BOp op e1 e2)
= def evalEA.2
evalBinOp op (evalEA e1) (evalEA e2)  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio 3 E
-- Ejercicio i
Prop: ¿para todo f. sizeT . mapT f = sizeT?
Dem: por ppio de ext. y por (.)
¿para todo f. para todo t. sizeT (mapT f t) = sizeT t?
sea h t'. Por ppio de ind. en la estructura de t':
CB t' = EmptyT) ¿sizeT (mapT h EmptyT) = sizeT EmptyT?
CI t' = NodeT x ti td) HI1) ¡sizeT (mapT h ti) = sizeT ti!
                       HI2) ¡sizeT (mapT h td) = sizeT td!
                       TI) ¿sizeT (mapT h (NodeT x ti td)) = sizeT (NodeT x ti td)?
Dem CB:
Lado izq:
sizeT (mapT h EmptyT)
= def mapT.1
sizeT EmptyT  -- VALE EL CASO
Dem CI:
Lado izq:
sizeT (mapT h (NodeT x ti td))
= def mapT.2
sizeT (NodeT (h x) (mapT h ti) (mapT h td))
= def sizeT.2
1 + sizeT (mapT h ti) + sizeT (mapT h td)
= HI1, HI2
1 + sizeT ti + sizeT td
Lado der:
sizeT (NodeT x ti td)
= def sizeT.2
1 + sizeT ti + sizeT td  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio ii
Prop: ¿para todo f. para todo g. mapT f . mapT g = mapT (f . g)?
Dem: por ppio de ext. y por (.)
¿para todo f. para todo g. para todo t. mapT f (mapT g t) = mapT (f . g) t?
sean h h' t'. Por ppio de ind. en la estructura de t':
CB t' = EmptyT) ¿mapT h (mapT h' EmptyT) = mapT (h . h') EmptyT?
CI t' = NodeT x ti td) HI1) ¡mapT h (mapT h' ti) = mapT (h . h') ti!
                       HI2) ¡mapT h (mapT h' td) = mapT (h . h') td!
                       TI) ¿mapT h (mapT h' (NodeT x ti td)) = mapT (h . h') (NodeT x ti td)?
Dem CB:
Lado izq:
mapT h (mapT h' EmptyT)
= def mapT.1
mapT h EmptyT
= def mapT.1
EmptyT
Lado der:
mapT (h . h') EmptyT
= def mapT.1
EmptyT  -- VALE EL CASO
Dem CI:
Lado izq:
mapT h (mapT h' (NodeT x ti td))
= def mapT.2
mapT h (NodeT (h' x) (mapT h' ti) (mapT h' td))
= def mapT.2
NodeT (h (h' x)) (mapT h (mapT h' ti)) (mapT h (mapT h' td))
= seccion de operadores, HI1, HI2
NodeT ((h . h') x) (mapT (h . h') ti) (mapT (h . h') td)
Lado der:
mapT (h . h') (NodeT x ti td)
= def mapT.2
NodeT ((h . h') x) (mapT (h . h') ti) (mapT (h . h') td)  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio iii
Prop: ¿foldT EmptyT NodeT = id?
Dem: por ppio de ext. y por id
¿para todo t. foldT EmptyT NodeT t = t?
sea t'. Por ppio de ind. en la estructura de t':
CB t' = EmptyT) ¿foldT EmptyT NodeT EmptyT = EmptyT?
CI t' = NodeT x ti td) HI1) ¡foldT EmptyT NodeT ti = ti!
                       HI2) ¡foldT EmptyT NodeT td = td!
                       TI) ¿foldT EmptyT NodeT (NodeT x ti td) = NodeT x ti td?
Dem CB:
Lado izq:
foldT EmptyT NodeT EmptyT
= def foldT.1
EmptyT  -- VALE EL CASO
Dem CI:
Lado izq:
foldT EmptyT NodeT (NodeT x ti td)
= def foldT.2
NodeT x (foldT EmptyT NodeT ti) (foldT EmptyT NodeT td)
= HI1, HI2
NodeT x ti td  -- VALE EL CASO VALE LA PROPIEDAD

-- Cuestionario
Prop: ¿sizeT = length . preOrder?
Dem: por ppio de ext. y por (.)
¿para todo t. sizeT t = length (preOrder t)?
sea t'. Por ppio de ind. en la estructura de t':
CB t' = EmptyT) ¿sizeT EmptyT = length (preOrder EmptyT)?
CI t' = NodeT x ti td) HI1) ¡sizeT ti = length (preOrder ti)!
                       HI2) ¡sizeT td = length (preOrder td)!
                       TI) ¿sizeT (NodeT x ti td) = length (preOrder (NodeT x ti td))?
Dem CB:
Lado izq:
sizeT EmptyT
= def sizeT.1
0
Lado der:
length (preOrder EmptyT)
= def preOrder.1
length []
= def length.1
0  -- VALE EL CASO
Dem CI:
Lado izq:
sizeT (NodeT x ti td)
= def sizeT.2
1 + sizeT ti + sizeT td
Lado der:
length (preOrder (NodeT x ti td))
= def preOrder.2
length ([x] ++ preOrder ti ++ preOrder td)
= seccion de operadores
length (x : [] ++ preOrder ti ++ preOrder td)
= def length.2
1 + length ([] ++ preOrder ti ++ preOrder td)
= def (++).1
1 + length (preOrder ti ++ preOrder td)
= Lema length-append
1 + (length (preOrder ti) + length (preOrder td))
= aritm., HI
1 + sizeT ti + sizeT td  -- VALE EL CASO VALE LA PROPIEDAD

Lema length-append: ¿para todo xs. para todo ys. length (xs ++ ys) = length xs + length ys?
Dem: sean zs ws. Por ppio de ind. en la estructura de zs:
CB zs = []) ¿length ([] ++ ws) = length [] + length ws?
CI zs = x:xs') HI) ¡length (xs' ++ ws) = length xs' + length ws!
               TI) ¿length ((x:xs') ++ ws) = length (x:xs') + length ws?
Dem CB:
Lado izq:
length ([] ++ ws)
= def (++).1
length ws
Lado der:
length [] + length ws
= def length.1
0 + length ws
= aritm.
length ws  -- VALE EL CASO
Dem CI:
Lado izq:
length ((x:xs') ++ ws)
= def (++).2
length (x : xs' ++ ws)
= def length.2
1 + length (xs' ++ ws)
Lado der:
length (x:xs') + length ws
= def length.2
1 + length xs' + length ws
= HI
1 + length (xs' ++ ws)  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio 5 B
Prop: ¿execute . compact = execute?
Dem: por ppio de ext. y por (.)
¿para todo q. execute (compact q) = execute q?
sea q'. Por ppio de ind. en la estructura de q':
CB q' = Table rs) ¿execute (compact (Table rs)) = execute (Table rs)?
CI.1 q' = Product q1 q2) HI11) ¡execute (compact q1) = execute q1!
                         HI21) ¡execute (compact q2) = execute q2!
                         TI1) ¿execute (compact (Product q1 q2)) = execute (Product q1 q2)?
CI.2 q' = Projection p q1) HI12) ¡execute (compact q1) = execute q1!
                           TI2) ¿execute (compact (Projection p q1)) = execute (Projection p q1)?
CI.3 q' = Selection p q1) HI13) ¡execute (compact q1) = execute q1!
                          TI3) ¿execute (compact (Selection p q1)) = execute (Selection p q1)?
Dem CB:
Lado izq:
execute (compact (Table rs))
= def compact.1
execute (Table rs)  -- VALE EL CASO
Dem CI.1:
Lado izq:
execute (compact (Product q1 q2))
= def compact.2
execute (Product (compact q1) (compact q2))
= def execute.2
product (execute (compact q1)) (execute (compact q2))
= HI11, HI21
product (execute q1) (execute q2)
Lado der:
execute (Product q1 q2)
= def execute.2
product (execute q1) (execute q2)  -- VALE EL CASO
Dem CI.2
Lado izq:
execute (compact (Projection p q1))
= def compact.3
execute (compactProjection p (compact q1))
= Lema execute-compactProjection
project p (execute (compact q1))
= HI12
project p (execute q1)
Lado der:
execute (Projection p q1)
= def execute.3
project p (execute q1)  -- VALE EL CASO
Dem CI.3
Lado izq:
execute (compact (Selection p q1))
= def compact.4
execute (compactSelection p (compact q1))
= Lema execute-compactSelection
select p (execute (compact q1))
= HI13
select p (execute q1)
Lado der:
execute (Selection p q1)
= def execute.4
select p (execute q1)  -- VALE EL CASO VALE LA PROPIEDAD

Lema execute-compactProjection: ¿para todo f. para todo q. execute (compactProjection f q) = project f (execute q)?
Dem: sean p q':
C1 q' = Projection p' q1) ¿execute (compactProjection p (Projection p' q1)) = project p (execute (Projection p' q1))?
C2 q' /= Projection p' q1) ¿execute (compactProjection p q') = project p (execute q')?
Dem C1:
Lado izq:
execute (compactProjection p (Projection p' q1))
= def compactProjection.1
execute (Projection (conjunct p p') q1)
= def execute.3
project (conjunct p p') (execute q1)
= Lema project-conjunct
project p (project p' (execute q1))
Lado der:
project p (execute (Projection p' q1))
= def execute.3
project p (project p' (execute q1))  -- VALE EL CASO
Dem C2:
Lado izq:
execute (compactProjection p q')
= def compactProjection.2
execute (Projection p q')
= def execute.3
project p (execute q')  -- VALE EL CASO VALE LA PROPIEDAD

Lema project-conjunct: ¿para todo f. para todo g. para todo xs. project f (project g xs) = project (conjunct f g) xs?
Dem: sean p1 p2 zs. Por ppio de ind. en la estructura de zs:
CB zs = []) ¿project p1 (project p2 []) = project (conjunct p1 p2) []?
CI zs = r:rs) HI) ¡project p1 (project p2 rs) = project (conjunct p1 p2) rs!
              TI) ¿project p1 (project p2 (r:rs)) = project (conjunct p1 p2) (r:rs)?
Dem CB:
Lado izq:
project p1 (project p2 [])
= def project.1
project p1 []
= def project.1
[]
Lado der:
project (conjunct p1 p2) []
= def project.1
[]  -- VALE EL CASO
Dem CI:
Lado izq:
project p1 (project p2 (r:rs))
= def project.2
project p1 (projectR p2 r : project p2 rs)
= def project.2
projectR p1 (projectR p2 r) : project p1 (project p2 rs)
= Lema projectR-conjunct
projectR (conjunct p1 p2) r : project p1 (project p2 rs)
Lado der:
project (conjunct p1 p2) (r:rs)
= def project.2
projectR (conjunct p1 p2) r : project (conjunct p1 p2) rs
= HI
projectR (conjunct p1 p2) r : project p1 (project p2 rs)  -- VALE EL CASO VALE LA PROPIEDAD

Lema projectR-conjunct: ¿para todo f. para todo g. para todo xs. projectR (conjunct f g) xs = projectR f (projectR g xs)?
Dem: sean p1 p2 zs. Por ppio de ind. en la estructura de zs:
CB zs = []) ¿projectR (conjunct p1 p2) [] = projectR p1 (projectR p2 [])?
CI zs = d:ds) HI) ¡projectR (conjunct p1 p2) ds = projectR p1 (projectR p2 ds)!
              TI) ¿projectR (conjunct p1 p2) (d:ds) = projectR p1 (projectR p2 (d:ds))?
Dem CB:
Lado izq:
projectR (conjunct p1 p2) []
= def projectR.1
[]
Lado der:
projectR p1 (projectR p2 [])
= def projectR.1
projectR p1 []
= def projectR.1
[]  -- VALE EL CASO
Dem CI:
Lado izq:
projectR (conjunct p1 p2) (d:ds)
= def projectR.2
if conjunct p1 p2 (fst d) then d : projectR (conjunct p1 p2) ds else projectR (conjunct p1 p2) ds
= def conjunct
if p1 (fst d) && p2 (fst d) then d : projectR (conjunct p1 p2) ds else projectR (conjunct p1 p2) ds
= HI, HI
if p1 (fst d) && p2 (fst d) then d : projectR p1 (projectR p2 ds) else projectR p1 (projectR p2 ds)
Lado der:
projectR p1 (projectR p2 (d:ds))
= def projectR.2
projectR p1 (if p2 (fst d) then d : projectR p2 ds else projectR p2 ds)
= Prop if-then-else
if p2 (fst d) then projectR p1 (d : projectR p2 ds) else projectR p1 (projectR p2 ds)
= def projectR.2
if p2 (fst d) then (if p1 (fst d) then d : projectR p1 (projectR p2 ds) else projectR p1 (projectR p2 ds)) else projectR p1 (projectR p2 ds)
= Lema de fusion de rama then de if
if p2 (fst d) && p1 (fst d) then d : projectR p1 (projectR p2 ds) else projectR p1 (projectR p2 ds)
= Lema conmutatividad-and
if p1 (fst d) && p2 (fst d) then d : projectR p1 (projectR p2 ds) else projectR p1 (projectR p2 ds)  -- VALE EL CASO VALE LA PROPIEDAD

Lema de fusion de rama then de if: ¿para todo b1. para todo b2. para todo x. para todo y. if b1 then if b2 then x else y else y = if b1 && b2 then x else y?
Dem: sean b1' b2' e e':
C1 b1' = True) ¿if True then if b2' then e else e' else e' = if True && b2' then e else e'?
C2 b1' = False) ¿if False then if b2' then e else e' else e' = if False && b2' then e else e'?
Dem C1:
Lado izq:
if True then if b2' then e else e' else e'
= eval-if
if b2' then e else e'
Lado der:
if True && b2' then e else e'
= def &&.2
if b2' then e else e'  -- VALE EL CASO
Dem C2:
Lado izq:
if False then if b2' then e else e' else e'
= eval-if
e'
Lado der:
if False && b2' then e else e'
= def &&.1
if False then e else e'
= eval-if
e'  -- VALE EL CASO VALE LA PROPIEDAD

Lema conmutatividad-and: ¿para todo b1. para todo b2. b1 && b2 = b2 && b1?
Dem: sean b1' b2':
C1 b1' = True y b2' = False) ¿True && False = False && True?
C2 b1' = False y b2' = True) ¿False && True = True && False?
Dem C1:
Lado izq:
True && False
= def &&.2
False
Lado der:
False && True
= def &&.1
False  -- VALE EL CASO
Dem C2:
Lado izq:
False && True
= def &&.1
False
Lado der:
True && False
= def &&.2
False  -- VALE EL CASO VALE LA PROPIEDAD

Lema execute-compactSelection: ¿para todo f. para todo q. execute (compactSelection f q) = select f (execute q)?
Dem: sean p q':
C1 q' = Selection p' q1) ¿execute (compactSelection p (Selection p' q1)) = select p (execute (Selection p' q1))?
C2 q' /= Selection p' q1) ¿execute (compactSelection p q') = select p (execute q')?
Dem C1:
Lado izq:
execute (compactSelection p (Selection p' q1))
= def compactSelection.1
execute (Selection (conjunct p p') q1)
= def execute.4
select (conjunct p p') (execute q1)
= Lema select-conjunct
select p (select p' (execute q1))
Lado der:
select p (execute (Selection p' q1))
= def execute.4
select p (select p' (execute q1))  -- VALE EL CASO
Dem C2:
Lado izq:
execute (compactSelection p q')
= def compactSelection.2
execute (Selection p q')
= def execute.4
select p (execute q')  -- VALE EL CASO VALE LA PROPIEDAD

Lema select-conjunct: ¿para todo f. para todo g. para todo xs. select f (select g xs) = select (conjunct f g) xs?
Dem: sean p1 p2 zs. Por ppio de ind. en la estructura de zs:
CB zs = []) ¿select p1 (select p2 []) = select (conjunct p1 p2) []?
CI zs = r:rs) HI) ¡select p1 (select p2 rs) = select (conjunct p1 p2) rs!
              TI) ¿select p1 (select p2 (r:rs)) = select (conjunct p1 p2) (r:rs)?
Dem CB:
Lado izq:
select p1 (select p2 [])
= def select.1
select p1 []
= def select.1
[]
Lado der:
select (conjunct p1 p2) []
= def select.1
[]  -- VALE EL CASO
Dem CI:
Lado izq:
select p1 (select p2 (r:rs))
= def select.2
select p1 (if p2 r then r : select p2 rs else select p2 rs)
= prop if-then-else
if p2 r then select p1 (r : select p2 rs) else select p1 (select p2 rs)
= def select.2
if p2 r then (if p1 r then r : select p1 (select p2 rs) else select p1 (select p2 rs)) else select p1 (select p2 rs)
= Lema de fusion de rama then de if
if p2 r && p1 r then r : select p1 (select p2 rs) else select p1 (select p2 rs)
= Lema conmutatividad-and
if p1 r && p2 r then r : select p1 (select p2 rs) else select p1 (select p2 rs)
Lado der:
select (conjunct p1 p2) (r:rs)
= def select.2
if conjunct p1 p2 r then r : select (conjunct p1 p2) rs else select (conjunct p1 p2) rs
= def conjunct, HI, HI
if p1 r && p2 r then r : select p1 (select p2 rs) else select p1 (select p2 rs)  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio 6 C
Prop: ¿para todo x. has (==x) = any (elem x) . objectsPerLevel?
Dem: por ppio de ext. y por (.)
¿para todo x. para todo m. has (==x) m = any (elem x) (objectsPerLevel m)?
sean o m'. Por ppio de ind. en la estructura de m':
CB m' = Cofre os) ¿has (==o) (Cofre os) = any (elem o) (objectsPerLevel (Cofre os))?
CI.1 m' = Nada m1) HI11) ¡has (==o) m1 = any (elem o) (objectsPerLevel m1)!
                   TI1) ¿has (==o) (Nada m1) = any (elem o) (objectsPerLevel (Nada m1))?
CI.2 m' = Bifurcacion os m1 m2) HI12) ¡has (==o) m1 = any (elem o) (objectsPerLevel m1)!
                                HI22) ¡has (==o) m2 = any (elem o) (objectsPerLevel m2)!
                                TI2) ¿has (==o) (Bifurcacion os m1 m2) = any (elem o) (objectsPerLevel (Bifurcacion os m1 m2))?
Dem CB:
Lado izq:
has (==o) (Cofre os)
= def has.1
any (==o) os
= Lema any-elem
elem o os
Lado der:
any (elem o) (objectsPerLevel (Cofre os))
= def objectsPerLevel.1
any (elem o) [os]
= seccion de operadores
any (elem o) (os:[])
= def any.2
elem o os || any (elem o) []
= def any.1
elem o os || False
= Lema conmutatividad-or
False || elem o os
= def ||.2
elem o os  -- VALE EL CASO
Dem CI.1:
Lado izq:
has (==o) (Nada m1)
= def has.2
has (==o) m1
Lado der:
any (elem o) (objectsPerLevel (Nada m1))
= def objectsPerLevel.2
any (elem o) ([] : objectsPerLevel m1)
= def any.2
elem o [] || any (elem o) (objectsPerLevel m1)
= def elem.1, HI11
False || has (==o) m1
= def ||.2
has (==o) m1  -- VALE EL CASO
Dem CI.2
Lado izq:
has (==o) (Bifurcacion os m1 m2)
= def has.3
any (==o) os || has (==o) m1 || has (==o) m2
Lado der:
any (elem o) (objectsPerLevel (Bifurcacion os m1 m2))
= def objectsPerLevel.3
any (elem o) (os : concatPerLevel (objectsPerLevel m1) (objectsPerLevel m2))
= def any.2
elem o os || any (elem o) (concatPerLevel (objectsPerLevel m1) (objectsPerLevel m2))
= Lema any-concatPerLevel
elem o os || any (elem o) (objectsPerLevel m1) || any (elem o) (objectsPerLevel m2)
= Lema any-elem, HI12, HI22
any (==o) os || has (==o) m1 || has (==o) m2  -- VALE EL CASO VALE LA PROPIEDAD

Lema any-elem: ¿para todo x. para todo xs. any (==x) xs = elem x xs?
Dem: sean e zs. Por ppio de ind. en la estructura de zs:
CB zs = []) ¿any (==e) [] = elem e []?
CI zs = x':xs') HI) ¡any (==e) xs' = elem e xs'!
                TI) ¿any (==e) (x':xs') = elem e (x':xs')?
Dem CB:
Lado izq:
any (==e) []
= def any.1
False
Lado der:
elem e []
= def elem.1
False  -- VALE EL CASO
Dem CI:
any (==e) (x':xs')
= def any.2
(==e') x' || any (==e) xs'
= seccion de operadores, HI
x==e' || elem e xs'
Lado der:
elem e (x':xs')
= def elem.2
x'==e || elem e xs'  -- VALE EL CASO VALE LA PROPIEDAD

Lema conmutatividad-or: ¿para todo b1. para todo b2. b1 || b2 = b2 || b1?
Dem: sea b1' b2'.
C1 b1' = True y b2' = False) ¿True || False = False || True?
C2 b1' = False y b2' = True) ¿False || True = True || False?
Dem C1:
Lado izq:
True || False
= def ||.1
True
Lado der:
False || True
= def ||.2
True  -- VALE EL CASO
Dem C2:
Lado izq:
False || True
= def ||.2
True
Lado der:
True || False
= def ||.1
True  -- VALE EL CASO VALE LA PROPIEDAD

Lema any-concatPerLevel: ¿para todo x. para todo xss. para todo yss. any (elem o) (concatPerLevel xss yss) = any (elem o) xss || any (elem o) yss?
Dem: sean o zss wss. Por ppio de ind. en la estructura de zss:
CB zss = []) ¿any (elem o) (concatPerLevel [] wss) = any (elem o) [] || any (elem o) wss?
CI zss = xs:xss')
 CI.1 wss = []) ¿any (elem o) (concatPerLevel (xs:xss') []) = any (elem o) (xs:xss') || any (elem o) []?
 CI.2 wss = ys:yss') HI) ¡any (elem o) (concatPerLevel xss' yss') = any (elem o) xss' || any (elem o) yss'!
                  TI) ¿any (elem o) (concatPerLevel (xs:xss') (ys:yss')) = any (elem o) (xs:xss') || any (elem o) (ys:yss')?
Dem CB:
Lado izq:
any (elem o) (concatPerLevel [] wss)
= df concatPerLevel.1
any (elem o) wss
Lado der:
any (elem o) [] || any (elem o) wss
= def any.1
False || any (elem o) wss
= def ||.2
any (elem o) wss  -- VALE EL CASO
Dem CI.1:
Lado izq:
any (elem o) (concatPerLevel (xs:xss') [])
= def concatPerLevel.2
any (elem o) (xs:xss')
Lado der:
any (elem o) (xs:xss') || any (elem o) []
= def any.1
any (elem o) (xs:xs') || False
= Lema conmutatividad-or
any (elem o) (xs:xss')  -- VALE EL CASO
Dem CI.2:
Lado izq:
any (elem o) (concatPerLevel (xs:xss') (ys:yss'))
= def concatPerLevel.3
any (elem o) ((xs ++ ys) : concatPerLevel xss' yss')
= def any.2
elem o (xs ++ ys) || any (elem o) (concatPerLevel xss' yss')
= HI
elem o (xs ++ ys) || any (elem o) xss' || any (elem o) yss'
Lado der:
any (elem o) (xs:xss') || any (elem o) (ys:yss')
= def any.2, def any.2
elem o xs || any (elem o) xss' || elem o ys || any (elem o) yss'
= Lema conmutatividad-or
elem o xs || elem o ys || any (elem o) xss' || any (elem o) yss'
= Lema elem-or
elem o (xs ++ ys) || any (elem o) xss' || any (elem o) yss'  -- VALE EL CASO VALE LA PROPIEDAD

Lama elem-or: ¿para todo x. para todo xs. para todo ys. elem o xs || elem o ys = elem o (xs++ys)?
Dem: sean o zs ws. Por ppio de ind. en la estructura de zs:
CB zs = []) ¿elem o [] || elem o ws = elem o ([]++ws)?
CI zs = x:xs') HI) ¡elem o xs' || elem o ws = elem o (xs'++ws)!
               TI) ¿elem o (x:xs') || elem o ws = elem o ((x:xs')++ws)?
Dem CB:
Lado izq:
elem o [] || elem o ws
= def elem.1
False || elem o ws
= def ||.2
elem o ws
Lado der:
elem o ([]++ws)
= def (++).1
elem 0 ws  -- VALE EL CASO
Dem CI:
elem o (x:xs') || elem o ws
= def elem.2
x==o || elem o xs' || elem o ws
= HI
x==o || elem o (xs'++ws)
Lado der:
elem o ((x:xs')++ws)
= def (++).2
elem o (x : xs' ++ ws)
= def elem.2
x==o || elem o (xs'++ws)  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio adicional
Prop: ¿para todo f. para todo t1. para todo t2. heightT (zipWithT f t1 t2) = min (heightT t1) (heightT t2)?
Dem: sean h t1' t2'. Por ppio de ind. en la estructura de t1':
CB t1' = EmptyT) ¿heightT (zipWithT h EmptyT t2') = min (heightT EmptyT) (heightT t2')?
CI t1' = NodeT x ti1 td1)
   CI.1 t2' = EmptyT) ¿heightT (zipWithT h (NodeT x ti1 td1) EmptyT) = min (heightT (NodeT x ti1 td1)) (heightT EmptyT)?
   CI.2 t2' = NodeT y ti2 td2) HI1) ¡heightT (zipWithT h ti1 ti2) = min (heightT ti1) (heightT ti2)!
                               HI2) ¡heightT (zipWithT h td1 td2) = min (heightT td1) (heightT td2)!
                               TI) ¿heightT (zipWithT h (NodeT x ti1 td1) (NodeT y ti2 td2)) = min (heightT (NodeT x ti1 td1)) (heightT (NodeT y ti2 td2))?
CB:
Lado izq:
heightT (zipWithT h EmptyT t2')
= def zipWithT.1
heightT EmptyT
= def heightT.1
0
Lado der:
min (heightT EmptyT) (heightT t2')
= def heightT.1
min 0 (heightT t2')
= def min
if 0 <= heightT t2' then 0 else heightT t2'  -- CONSULTAR
= def <=
if True then 0 else heightT t2'
= eval-if
0  -- VALE EL CASO
Dem CI.1:
Lado izq:
heightT (zipWithT h (NodeT x ti1 td1) EmptyT)
= def zipWithT.2
heightT EmptyT
= def heightT.1
0
Lado der:
min (heightT (NodeT x ti1 td1)) (heightT EmptyT)
= def heightT.2, def heightT.1
min (1 + heightT ti1 `max` heightT td2) 0  -- CONSULTAR
= def min
if 1 + heightT ti1 `max` heightT td2 <= 0 then 1 + heightT ti1 `max` heightT td2 else 0
= def <=
if False then 1 + heightT ti1 `max` heightT td2 else 0
= eval-if
0  -- VALE EL CASO
Dem CI.2:
Lado izq:
heightT (zipWithT h (NodeT x ti1 td1) (NodeT y ti2 td2))
= def zipWithT.3
heightT (NodeT (h x y) (zipWithT h ti1 ti2) (zipWithT h td1 td2))
= def heightT.2
1 + heightT (zipWithT h ti1 ti2) `max` heightT (zipWithT h td1 td2)
= HI1, HI2
1 + min (heightT ti1) (heightT ti2) `max` min (heightT td1) (heightT td2)
= seccion de operadores
1 + max (min (heightT ti1) (heightT ti2)) (min (heightT td1) (heightT td2))
Lado der:
min (heightT (NodeT x ti1 td1)) (heightT (NodeT y ti2 td2))
= def heightT.2, def heightT.2
min (1 + heightT ti1 `max` heightT td1) (1 + heightT ti2 `max` heightT td2)
Lema sum-min
1 + min (heightT ti1 `max` heightT td1) (heightT ti2 `max` heightT td2)
= seccion de operadores
1 + min (max (heightT ti1) (heightT td1)) (max (heightT ti2) (heightT td2))
= Lema max-min
1 + max (min (heightT ti1) (heightT ti2)) (min (heightT td1) (heightT td2))  -- VALE EL CASO VALE LA PROPIEDAD

Lema sum-min : ¿para todo n1. para todo n2. para todo n3. min (n1+n2) (n1+n3) = n1 + min n2 n3? -- CONSULTAR
Dem: ...

Lema max-min : ¿para todo n1. para todo n2. para todo n3. para todo n4. max (min n1 n2) (min n3 n4) = min (max n1 n3) (max n2 n4)? -- CONSULTAR
Dem: ...

Lema de fusion de rama else de if: ¿para todo b1. para todo b2. para todo x. para todo y. if b1 then x else if b2 then x else y = if b1 || b2 then x else y?
Dem: sean b1' b2' e e':
C1 b1' = True) ¿if True then e else if b2' then e else e' = if True || b2' then e else e'?
C2 b1' = False) ¿if False then e else if b2' then e else e' = if False || b2' then e else e'?
Dem C1:
Lado izq:
if True then e else if b2' then e else e'
= eval-if
e
Lado der:
if True || b2' then e else e'
= def ||.1
e  -- VALE EL CASO
Dem C2:
Lado izq:
if False then e else if b2' then e else e'
= eval-if
if b2' then e else e'
Lado der:
if False || b2' then e else e'
= def ||.2
if b2' then e else e'  -- VALE EL CASO VALE LA PROPIEDAD

data Dir' = Izq | Der
     deriving Show

--Tarea:
caminoMasCortoHasta :: Eq a => a -> Tree a -> Maybe [Dir']
-- PROP: Devuelve una lista de direcciones al camino mas corto hasta cierto elemento.
caminoMasCortoHasta e t = snd (distanciaYCaminoM e t)

distanciaYCaminoM :: Eq a => a -> Tree a -> (Int, Maybe [Dir'])
distanciaYCaminoM e = foldT (0,Nothing) (\x nd1 nd2 -> if x==e then (0,Just []) else agregarDirM nd1 nd2)

agregarDirM :: (Int, Maybe [Dir']) -> (Int, Maybe [Dir']) -> (Int, Maybe [Dir'])
agregarDirM (n,Just ds1) (m,Just ds2) = if n>m then (m+1,Just (Der:ds2)) else (n+1,Just (Izq:ds1))
agregarDirM (n,Just ds)  _            = (n+1,Just (Izq:ds))
agregarDirM _            (m,Just ds)  = (m+1,Just (Der:ds))
agregarDirM _            _            = (0,Nothing)

estaEnElCamino :: [Dir'] -> Tree a -> Bool
-- PROP: Dado un camino y un arbol y me dice si esta o no donde apunta la lista de direcciones.
estaEnElCamino = foldr (\d h -> \t -> case t of EmptyT -> False; NodeT _ ti td -> case d of Izq -> h ti; _ -> h td)
                       (\t -> case t of EmptyT -> False; _ -> True)

todosLosCaminosDeDirs :: Tree a -> [[Dir']]
-- PROP: Devuelve una lista de lista de todas las direcciones posibles que hay en el arbol dado, incluyendo empty.
todosLosCaminosDeDirs = foldT [[]] (\_ ds1 ds2 -> agregarDir Izq ds1 ++ agregarDir Der ds2)

agregarDir :: Dir' -> [[Dir']] -> [[Dir']]
agregarDir _ []       = []
agregarDir d (ds:dss) = (d : ds) : agregarDir d dss

todosLosCaminosDeDirsHasta :: Eq a => a -> Tree a -> [[Dir']]
-- PROP: Devuelve todos los caminos que llevarian a esos elementos, el elemento puede estar repetido.
todosLosCaminosDeDirsHasta e = foldT []
                                     (\x ds1 ds2 -> if e==x then [] : agregarDir Izq ds1 ++ agregarDir Der ds2 else agregarDir Izq ds1 ++ agregarDir Der ds2)

countByT :: (a -> Bool) -> Tree a -> Int
countByT _ EmptyT          = 0
countByT p (NodeT x ti td) = unoSi (p x) + countByT p ti + countByT p td

todosLosCaminosDeDirsHasta :: Eq a => a -> Tree a -> [[Dir']]
todosLosCaminosDeDirsHasta e EmptyT          = []
todosLosCaminosDeDirsHasta e (NodeT x ti td) = let ds = agregarDir Izq (todosLosCaminosDeDirsHasta e ti) ++ agregarDir Der (todosLosCaminosDeDirsHasta e td)
                                                in if e==x then [] : ds else ds

Prop: ¿para todo e. countByT (e==) = length . todosLosCaminosDeDirsHasta e?
Dem: por ppio de ext. y por (.)
¿para todo e. para todo t. countByT (e==) t = length (todosLosCaminosDeDirsHasta e t)?
sean e' t'. Por ppio de ind. en la estructura de t':
CB t' = EmptyT) ¿countByT (e'==) EmptyT = length (todosLosCaminosDeDirsHasta e' EmptyT)?
CI t' = NodeT x ti td) HI1) ¡countByT (e'==) ti = length (todosLosCaminosDeDirsHasta e' ti)!
                       HI2) ¡countByT (e'==) td = length (todosLosCaminosDeDirsHasta e' td)!
                       TI) ¿countByT (e'==) (NodeT x ti td) = length (todosLosCaminosDeDirsHasta e' (NodeT x ti td))?
Dem CB:
Lado izq:
countByT (e'==) EmptyT
= def countByT.1
0
Lado der:
length (todosLosCaminosDeDirsHasta e' EmptyT)
= def todosLosCaminosDeDirsHasta.1
length []
= def length
0  -- VALE EL CASO
Dem CI:
Lado izq:
countByT (e'==) (NodeT x ti td)
= def countByT.2
unoSi ((e'==) x) + countByT (e'==) ti + countByT (e'==) td
= seccion de operadores
unoSi (e'==x) + countByT (e'==) ti + countByT (e'==) td
= Lema unoSi-if
if e'==x then 1 + countByT (e'==) ti + countByT (e'==) td else countByT (e'==) ti + countByT (e'==) td
Lado der:
length (todosLosCaminosDeDirsHasta e' (NodeT x ti td))
= def todosLosCaminosDeDirsHasta.2
length (let ds = agregarDir Izq (todosLosCaminosDeDirsHasta e' ti) ++ agregarDir Der (todosLosCaminosDeDirsHasta e' td) in if e'==x then [] : ds else ds)
= def ds
length (if e'==x
         then [] : agregarDir Izq (todosLosCaminosDeDirsHasta e' ti) ++ agregarDir Der (todosLosCaminosDeDirsHasta e' td)
         else agregarDir Izq (todosLosCaminosDeDirsHasta e' ti) ++ agregarDir Der (todosLosCaminosDeDirsHasta e' td))
= prop of-then-else
if e'==x
 then length ([] : agregarDir Izq (todosLosCaminosDeDirsHasta e' ti) ++ agregarDir Der (todosLosCaminosDeDirsHasta e' td))
 else length (agregarDir Izq (todosLosCaminosDeDirsHasta e' ti) ++ agregarDir Der (todosLosCaminosDeDirsHasta e' td))
= def length.2
if e'==x
 then 1 + length (agregarDir Izq (todosLosCaminosDeDirsHasta e' ti) ++ agregarDir Der (todosLosCaminosDeDirsHasta e' td))
 else length (agregarDir Izq (todosLosCaminosDeDirsHasta e' ti) ++ agregarDir Der (todosLosCaminosDeDirsHasta e' td))
= Lema practica 8.1.2.a
if e'==x
 then 1 + length (agregarDir Izq (todosLosCaminosDeDirsHasta e' ti)) + length (agregarDir Der (todosLosCaminosDeDirsHasta e' td))
 else length (agregarDir Izq (todosLosCaminosDeDirsHasta e' ti)) + length (agregarDir Der (todosLosCaminosDeDirsHasta e' td))
= Lema length-agregarDir
if e'==x
 then 1 + length (todosLosCaminosDeDirsHasta e' ti) + length (todosLosCaminosDeDirsHasta e' td)
 else length (todosLosCaminosDeDirsHasta e' ti) + length (todosLosCaminosDeDirsHasta e' td)
= HI1, HI2, HI1, HI2
if e'==x then 1 + countByT (e'==) ti + countByT (e'==) td else countByT (e'==) ti + countByT (e'==) td  -- VALE EL CASO VALE LA PROPIEDAD

Lema length-agregarDir: ¿para todo d. para todo xss. length (agregarDir d xss) = length xss?
Dem: sean d' dss. Por ppio de ind. en la estructura de dss:
CB dss = []) ¿length (agregarDir d' []) = length []?
CI dss = ds:dss') HI) ¡length (agregarDir d' dss') = length dss'!
                  TI) ¿length (agregarDir d' (ds:dss')) = length (ds:dss')?
Dem CB:
Lado izq:
length (agregarDir d' [])
= def agregarDir.1
length []  -- VALE EL CASO
Dem CI:
LAdo izq:
length (agregarDir d' (ds:dss'))
= def agregarDir.2
length ((d' : ds) : agregarDir d' dss')
= def length.2
1 + length (agregarDir d' dss')
= HI
1 + length dss'
Lado der:
length (ds:dss')
= def length.2
1 + length dss'  -- VALE EL CASO VALE LA PROPIEDAD

Lema unoSi-if: ¿para todo b. para todo n. unoSi b + n = if b then 1 + n else n?
Dem: sean b' y m:
C1 b' = True) ¿unoSi True + m = if True then 1 + m else m?
C2 b' = False) ¿unoSi False + m = if False then 1 + m else m?
Dem C1:
Lado izq:
unoSi True + m
= def unoSi.1
1 + m
Lado der:
if True then 1 + m else m
= eval-if
1 + m  -- VALE EL CASO
Dem C2:
Lado izq:
unoSi False + m
= def unoSi.2
0 + m
= aritm.
m
Lado der:
if False then 1 + m else m
= eval-if
m  -- VALE EL CASO VALE LA PROPIEDAD