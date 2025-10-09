-- Ejercicio 1
data N = Z | S N
     deriving Show

evalN :: N -> Int
evalN Z     = 0
evalN (S n) = 1 + evalN n

addN :: N -> N -> N
addN Z      n = n
addN (S n') n = S (addN n' n)

prodN :: N -> N -> N
prodN Z      n = Z
prodN (S n') n = addN n (prodN n' n)

int2N :: Int -> N
int2N 0 = Z
int2N n = S (int2N (n-1))

-- Ejercicio 2
type NU = [()]
data Unit = Unit

evalNU :: NU -> Int
evalNU []     = 0
evalNU (_:ns) = 1 + evalNU ns
-- alternativa: evalNu nu = length nu

succNU :: NU -> NU
succNU n = () : n
-- no es por recursion estructural

addNU :: NU -> NU -> NU
addNU []     m = m
addNU (n:ns) m = n : addNU ns m
-- alternativa: addNu nu nu' = nu ++ nu'

nu2n :: NU -> N
nu2n []     = Z
nu2n (_:ns) = S (nu2n ns)

n2nu :: N -> NU
n2nu Z     = []
n2nu (S n) = () : (n2nu n)

-- Demos
-- Ejercicio I
-- Prop.: ¿evalNU . succNU = (+1) . evalNU?
-- Dem.: por ppio. de ext.
-- ¿(evalNU . succNU) n = ((+1) . evalNU) n?
-- Sea m una elemento totalmente definido de NU. Por ppio. de ind. en la estructura de m.
-- Caso 1, m = []) ¿(evalNU . succNU) [] = ((+1) . evalNU) []?
-- Caso 2, m = n':ns)¿(evalNU . succNU) (n':ns) = ((+1) . evalNU) (n':ns)?
-- Dem.: Caso 1
-- Lado izq.:
-- (evalNU . succNU) []
-- = def. (.)
-- evalNU (succNU [])
-- = def. succNU
-- evalNU (():[])
-- = def. evalNU.2
-- 1 + evalNU []
-- = def. evalNU.1
-- 1 + 0
-- = aritm.
-- 1
-- Lado der.:
-- ((+1) . evalNU) []
-- = def (.)
-- (+1) (evalNU [])
-- = def. evalNU.1
-- (+1) 0
-- = aritm.
-- 1
-- Dem.: Caso 2
-- Lado izq.:
-- (evalNU . succNU) (n':ns)
-- = def. (.)
-- evalNU (succNU (n':ns))
-- = def. succNU
-- evalNU (():n':ns)
-- = def. evalNU.2
-- 1 + evalNU (n':ns)
-- = def. evalNU.2
-- 1 + (1 + evalNU ns)
-- Lado der.:
-- ((+1) . evalNU) (n':ns)
-- = def. (.)
-- (+1) (evalNU (n':ns))
-- = def. evalNU.2
-- (+1) (1 + evalNU ns)
-- = reescribo (+1)
-- 1 + (1 + evalNU ns)

-- Ejercicio II
-- Prop.: ¿para todo n1. para todo n2. evalNU (addNU n1 n2) = evalNU n1 + evalNU n2?
-- Dem.: Sea m1 m2 dos elementos totalmente definidos de NU. Por ppio. de ind. en la estructura de m1.
-- Caso base, m1 = []) ¿evalNU (addNU [] m2) = evalNU [] + evalNU m2?
-- Caso ind., m1 = n:ns) HI) ¡evalNU (addNU ns m2) = evalNU ns + evalNU m2!
--                       TI) ¿evalNU (addNU (n:ns) m2) = evalNU (n:ns) + evalNU m2?
-- Dem.: Caso base
-- Lado izq.:
-- evalNU (addNU [] m2)
-- = def. addNU.1
-- evalNU m2
-- Lado der.:
-- evalNU [] + evalNU m2
-- = def. evalNU.1
-- 0 + evalNU m2
-- = artim.
-- evalNU m2
-- Dem.: Caso ind.
-- Lado izq.:
-- evalNU (addNU (n:ns) m2)
-- = def. addNU.2
-- evalNU (n : addNU ns m2)
-- = def. evalNU.2
-- 1 + evallNU (addNU ns m2)
-- = HI
-- 1 + (evalNU ns + evalNU m2)
-- Lado der.:
-- evalNU (n:ns) + evalNU m2
-- = def. evalNU.2
-- (1 + evalNU ns) + evalNU m2
-- = aritm.
-- 1 + (evalNU ns + evalNU m2)

-- Ejercicio III
-- Prop.: ¿nu2n . n2nu = id?
-- Dem.: por ppio. de ext.
-- ¿(nu2n . n2nu) n = id n?
-- Sea m un elemento totalmente definido de N. Por ppio. de ind. en la estructura de m.
-- Caso base, m = Z) ¿(nu2n . n2nu) Z = id Z?
-- Caso ind., m = S n') HI) ¡(nu2n . n2nu) n' = id n'!
--                      TI) ¿(nu2n . n2nu) (S n') = id (S n')?
-- Dem.: Caso base
-- Lado izq.:
-- (nu2n . n2nu) Z
-- = def. (.)
-- nu2n (n2nu Z)
-- = def. n2nu.1
-- nu2n []
-- def. nu2n.1
-- Z
-- Lado der.:
-- id Z
-- = def. id
-- Z
-- Dem.: Caso ind.
-- Lado izq.:
-- (nu2n . n2nu) (S n')
-- = def. (.)
-- nu2n (n2nu (S n'))
-- = def. n2nu.2
-- nu2n (() : (n2nu n))
-- = def. nu2n.2
-- S (nu2n n)
-- Lado der.:
-- id (S n')
-- = def. id
-- S n'

-- Ejercicio IV
-- Prop.: ¿n2nu . nu2n = id?

-- Ejercicio 3
type NBin = [DigBin]
data DigBin = O | I
     deriving Show

dbAsInt :: DigBin -> Int
dbAsInt O = 0
dbAsInt _ = 1

dbAsBool :: DigBin -> Bool
dbAsBool O = False
dbAsBool _ = True

dbOfBool :: Bool -> DigBin
dbOfBool False = O
dbOfBool _     = I

negDB :: DigBin -> DigBin
negDB O = I
negDB _ = O

evalNB :: NBin -> Int
evalNB []     = 0
evalNB (d:ds) = dbAsInt d + 2 * evalNB ds

normalizarNB :: NBin -> NBin
normalizarNB []     = []
normalizarNB (d:ds) = normalizarDB d (normalizarNB ds)

normalizarDB :: DigBin -> NBin -> NBin
normalizarDB O [] = []
normalizarDB d ds = d : ds

succNB :: NBin -> NBin
succNB []     = I : []
succNB (O:ds) = I : ds
succNB (I:ds) = O : succNB ds

addNB :: NBin -> NBin -> NBin
addNB []       nb       = nb
addNB nb       []       = nb
addNB (d1:ds1) (d2:ds2) = addDB d1 d2 (addNB ds1 ds2)

addDB :: DigBin -> DigBin -> NBin -> NBin
addDB O  d2 ds = d2 : ds
addDB d1 O  ds = d1 : ds
addDB I  I  ds = O : succNB ds

nb2n :: NBin -> N
nb2n []     = Z
nb2n (d:ds) = addN (dbAsN d) (prodN (S (S Z)) (nb2n ds))

dbAsN :: DigBin -> N
dbAsN O = Z
dbAsN _ = S Z

n2nb :: N -> NBin
n2nb Z     = []
n2nb (S n) = succNB (n2nb n)