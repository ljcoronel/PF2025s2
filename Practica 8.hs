-- Seccion II
-- Ejercicio 1
data N = Z | S N
     deriving Show

-- Ejercicio i
evalN :: N -> Int
evalN Z     = 0
evalN (S n) = 1 + evalN n

-- Ejercicio ii
addN :: N -> N -> N
addN Z      n = n
addN (S n') n = S (n' `addN` n)

-- Ejercicio iii
prodN :: N -> N -> N
prodN Z      n = Z
prodN (S n') n = n `addN` (n' `prodN` n)

-- Ejercicio iv
int2N :: Int -> N
int2N 0 = Z
int2N n = S (int2N (n-1))

-- Ejercicio 2
type NU = [()]
data Unit = Unit

-- Ejercicio i
evalNU :: NU -> Int
evalNU []     = 0
evalNU (_:ns) = 1 + evalNU ns

-- Ejercicio ii
succNU :: NU -> NU
succNU n = () : n

-- Ejercicio iii
addNU :: NU -> NU -> NU
addNU []     m = m
addNU (n:ns) m = n : addNU ns m

-- Ejercicio iv
nu2n :: NU -> N
nu2n []     = Z
nu2n (_:ns) = S (nu2n ns)

-- Ejercicio v
n2nu :: N -> NU
n2nu Z     = []
n2nu (S n) = () : (n2nu n)

-- Ejercicio 3
type NBin = [DigBin]
data DigBin = O | I
     deriving Show

-- Ejercicio i
evalNB :: NBin -> Int
evalNB []     = 0
evalNB (d:ds) = dbAsInt d + 2 * evalNB ds

dbAsInt :: DigBin -> Int
dbAsInt O = 0
dbAsInt _ = 1

-- Ejercicio ii
normalizarNB :: NBin -> NBin
normalizarNB []     = []
normalizarNB (d:ds) = normalizarDB d (normalizarNB ds)

normalizarDB :: DigBin -> NBin -> NBin
normalizarDB O [] = []
normalizarDB d ds = d : ds

-- Ejercicio iii
succNB :: NBin -> NBin
succNB []     = I : []
succNB (O:ds) = I : ds
succNB (I:ds) = O : succNB ds

-- Ejercicio iv
addNB :: NBin -> NBin -> NBin
addNB n1 n2 = addNBConCarry n1 n2 O

addNBConCarry :: NBin -> NBin -> DigBin -> NBin
addNBConCarry []       []       carry = if dbAsBool carry
                                         then [carry]
                                         else []
addNBConCarry []       (d:ds)   carry = let (r,c) = addDBConCarry O d carry
                                         in r : addNBConCarry [] ds c
addNBConCarry (d:ds)   []       carry = let (r,c) = addDBConCarry d O carry
                                         in r : addNBConCarry ds [] c
addNBConCarry (d1:ds1) (d2:ds2) carry = let (r,c) = addDBConCarry d1 d2 carry
                                         in r : addNBConCarry ds1 ds2 c

addDBConCarry :: DigBin -> DigBin -> DigBin -> (DigBin, DigBin)
addDBConCarry O O O = (O,O)
addDBConCarry O O I = (I,O)
addDBConCarry O I O = (I,O)
addDBConCarry O I I = (O,I)
addDBConCarry I O O = (I,O)
addDBConCarry I O I = (O,I)
addDBConCarry I I O = (O,I)
addDBConCarry I I I = (I,I)

dbAsBool :: DigBin -> Bool
dbAsBool O = False
dbAsBool _ = True

-- Ejercicio v
nb2n :: NBin -> N
nb2n []     = Z
nb2n (d:ds) = dbAsN d (S (S Z) `prodN` nb2n ds)

dbAsN :: DigBin -> N -> N
dbAsN O = id
dbAsN _ = S

-- Ejercicio vi
n2nb :: N -> NBin
n2nb Z     = []
n2nb (S n) = succNB (n2nb n)

-- Seccion III
-- Ejercicio 1
data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA
     deriving Show

-- Ejercicio i
evalExpA :: ExpA -> Int
evalExpA (Cte  n)    = n
evalExpA (Suma e e') = evalExpA e + evalExpA e'
evalExpA (Prod e e') = evalExpA e * evalExpA e'

-- Ejercicio ii
simplificarExpA :: ExpA -> ExpA
simplificarExpA (Cte  n)    = Cte n
simplificarExpA (Suma e e') = simplificarSumaExpA (simplificarExpA e) (simplificarExpA e')
simplificarExpA (Prod e e') = simplificarProdExpA (simplificarExpA e) (simplificarExpA e')

simplificarSumaExpA :: ExpA -> ExpA -> ExpA
simplificarSumaExpA (Cte 0) e       = e
simplificarSumaExpA e       (Cte 0) = e
simplificarSumaExpA e       e'      = Suma e e'

simplificarProdExpA :: ExpA -> ExpA -> ExpA
simplificarProdExpA (Cte 0) _       = Cte 0
simplificarProdExpA _       (Cte 0) = Cte 0
simplificarProdExpA (Cte 1) e       = e
simplificarProdExpA e       (Cte 1) = e
simplificarProdExpA e       e'      = Prod e e'

-- Ejercicio iii
cantidadDeSumaCero :: ExpA -> Int
cantidadDeSumaCero (Cte  _)    = 0
cantidadDeSumaCero (Suma e e') = unoSiSumaCero e e' + cantidadDeSumaCero e + cantidadDeSumaCero e'
cantidadDeSumaCero (Prod e e') = cantidadDeSumaCero e + cantidadDeSumaCero e'

unoSiSumaCero :: ExpA -> ExpA -> Int
unoSiSumaCero (Cte 0) _       = 1
unoSiSumaCero _       (Cte 0) = 1
unoSiSumaCero _       _       = 0

-- Ejercicio 2
data ExpS = CteS N | SumS ExpS ExpS | ProdS ExpS ExpS
     deriving Show

-- Ejercicio i
evalES :: ExpS -> Int
evalES (CteS  n)    = evalN n
evalES (SumS  e e') = evalES e + evalES e'
evalES (ProdS e e') = evalES e * evalES e'

-- Ejercicio ii
es2ExpA :: ExpS -> ExpA
es2ExpA (CteS  n)    = Cte (evalN n)
es2ExpA (SumS  e e') = Suma (es2ExpA e) (es2ExpA e')
es2ExpA (ProdS e e') = Prod (es2ExpA e) (es2ExpA e')

-- Ejercicio iii
expA2es :: ExpA -> ExpS
expA2es (Cte  n)    = CteS (int2N n)
expA2es (Suma e e') = SumS (expA2es e) (expA2es e')
expA2es (Prod e e') = ProdS (expA2es e) (expA2es e')

-- Ejercicios adicionales
data Pot2 = Uno | Dos Pot2
     deriving Show

evalPot2 :: Pot2 -> Int
evalPot2 Uno     = 1
evalPot2 (Dos n) = 2 * evalPot2 n

multPot2 :: Pot2 -> Pot2 -> Pot2
multPot2 Uno     p = p
multPot2 (Dos n) p = Dos (multPot2 n p)

laMasLarga :: [[a]] -> [a]
laMasLarga []       = []
laMasLarga (xs:xss) = if length xs > length (laMasLarga xss)
                       then xs
                       else laMasLarga xss

elMas :: (a -> Int) -> [a] -> a
-- PRECOND: La lista no es vacia.
elMas _ []     = error "La lista dada es vacia"
elMas _ (x:[]) = x
elMas f (x:xs) = if f x > f (elMas f xs)
                  then x
                  else elMas f xs

laMasLarga' :: [[a]] -> [a]
-- PRECOND: La lista no es vacia.
laMasLarga' = elMas length
