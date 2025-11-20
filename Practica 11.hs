-- Ejercicio 1
data Pizza = Prepizza | Capa Ingrediente Pizza
     deriving Show
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa
     deriving Show

-- Ejercicio A
cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen _ Prepizza    = 0
cantidadCapasQueCumplen p (Capa i pz) = unoSi (p i) + cantidadCapasQueCumplen p pz

unoSi :: Bool -> Int
unoSi True = 1
unoSi _    = 0

-- Ejercicio B
conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas _ Prepizza    = Prepizza
conCapasTransformadas f (Capa i pz) = Capa (f i) (conCapasTransformadas f pz)

-- Ejercicio C
soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue _ Prepizza    = Prepizza
soloLasCapasQue p (Capa i pz) = if p i
                                 then Capa i (soloLasCapasQue p pz)
                                 else soloLasCapasQue p pz

-- Ejercicio 2
-- Ejercicio A
sinLactosa :: Pizza -> Pizza
sinLactosa = soloLasCapasQue (not . esQueso)

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False

-- Ejercicio B
aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa = (==0) . cantidadCapasQueCumplen esQueso

-- Ejercicio C
cantidadDeQueso :: Pizza -> Int
cantidadDeQueso = cantidadCapasQueCumplen esQueso

-- Ejercicio D
conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = conCapasTransformadas duplicarAceitunas

duplicarAceitunas :: Ingrediente -> Ingrediente
duplicarAceitunas (Aceitunas n) = Aceitunas (n*2)
duplicarAceitunas i             = i

-- Ejercicio 3
pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada _ z Prepizza    = z
pizzaProcesada f z (Capa i pz) = f i (pizzaProcesada f z pz)

-- Ejercicio 4
cantidadCapasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen' p = pizzaProcesada ((+) . unoSi . p) 0

conCapasTransformadas' :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas' f = pizzaProcesada (Capa . f) Prepizza

soloLasCapasQue' :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' p = pizzaProcesada (\i pz -> if p i then Capa i pz else pz) Prepizza

sinLactosa' :: Pizza -> Pizza
sinLactosa' = pizzaProcesada (\i pz -> if (not . esQueso) i then Capa i pz else pz) Prepizza

aptaIntolerantesLactosa' :: Pizza -> Bool
aptaIntolerantesLactosa' = pizzaProcesada ((&&) . not . esQueso) True
                                          --(\i b -> (not . esQueso) i && b)

cantidadDeQueso' :: Pizza -> Int
cantidadDeQueso' = pizzaProcesada ((+) . unoSi . esQueso) 0
                                  --(\i n -> unoSi (esQueso i) + n)

conElDobleDeAceitunas' :: Pizza -> Pizza
conElDobleDeAceitunas' = pizzaProcesada (Capa . duplicarAceitunas) Prepizza
                                        --(\i pz -> Capa (duplicarAceitunas i) pz)

-- Ejercicio 5
-- Ejercicio A
cantidadAceitunas :: Pizza -> Int
cantidadAceitunas = pizzaProcesada ((+) . aceitunas) 0
                                   --(\i n -> aceitunas i + n)

aceitunas :: Ingrediente -> Int
aceitunas (Aceitunas n) = n
aceitunas _             = 0

-- Ejercicio B
capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen p = pizzaProcesada (\i is -> if p i then i : is else is) []

-- Ejercicio C
conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada = pizzaProcesada juntarAceitunas Prepizza
                                        --(\i pz -> juntarAceitunas i pz)

juntarAceitunas :: Ingrediente -> Pizza -> Pizza
juntarAceitunas (Aceitunas n) (Capa (Aceitunas m) pz) = Capa (Aceitunas (n+m)) pz
juntarAceitunas i             pz                      = Capa i pz

-- Ejercicio D
conCapasDe :: Pizza -> Pizza -> Pizza
conCapasDe = pizzaProcesada ((.) . Capa) id
                            --(\i h -> \pz -> Capa i (h pz))

-- Ejercicio E
primerasNCapas :: Int -> Pizza -> Pizza
primerasNCapas = flip (pizzaProcesada (\i h -> \n -> if n==0 then Prepizza else Capa i (h (n-1))) (const Prepizza))

-- Ejercicio 7
-- Ejercicio A
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

-- Ejercicio B
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' p (x:xs) = if p x
                    then x : filter' p xs
                    else filter' p xs

-- Ejercicio C
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z []     = z
foldr' f z (x:xs) = f x (foldr' f z xs)

-- Ejercicio D
recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z _ []     = z
recr z f (x:xs) = f x xs (recr z f xs)

-- Ejercicio E
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ []     = error "Lista vacia"
foldr1' _ (x:[]) = x
foldr1' f (x:xs) = f x (foldr1' f xs)

-- Ejercicio F
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []     _      = []
zipWith' _ _      []     = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Ejercicio G
scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' _ z []     = [z]
scanr' f z (x:xs) = let rs = scanr' f z xs
                     in f x (head rs) : rs

-- Ejercicio 9
-- Ejercicio A
sum :: [Int] -> Int
sum = foldr' (+) 0
           --(\n m -> n + m)

-- Ejercicio B
length :: [a] -> Int
length = foldr' (const (1+)) 0
              --(\_ n -> 1 + n)

-- Ejercicio C
map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr' ((:) . f) []
               --(\x ys -> (f x) : ys)

-- Ejercicio D
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr' (\x xs -> if p x then x : xs else xs) []

-- Ejercicio E
find :: (a -> Bool) -> [a] -> Maybe a
find p = foldr' (\x m -> if p x then Just x else m) Nothing

-- Ejercicio F
any :: (a -> Bool) -> [a] -> Bool
any p = foldr' ((||) . p) False
             --(\x b -> p x || b)

-- Ejercicio G
all :: (a -> Bool) -> [a] -> Bool
all p = foldr' ((&&) . p) True
             --(\x b -> p x && b)
-- Ejercicio H
countBy :: (a -> Bool) -> [a] -> Int
countBy p = foldr' ((+) . unoSi . p) 0
                 --(\x n -> unoSi (p x) + n)

-- Ejercicio I
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p = foldr' (\x (xs,ys) -> if p x then (x:xs,ys) else (xs,x:ys)) ([],[])

-- Ejercicio J
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f = foldr' (\x h -> \ys -> case ys of [] -> []; (y:ys) -> f x y : h ys) (const [])

-- Ejercicio K
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f e = foldr' ((\x ys -> f x (head ys) : ys)) [e]
                  --(\x ys -> (:) (f x (head ys)))
                  --(\x ys -> (:) (f x . head ys))

-- Ejercicio L
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr' (\x xs -> if p x then x : xs else []) []

-- Ejercicio M
take :: Int -> [a] -> [a]
take = flip (foldr' (\x h -> \n -> if n==0 then [] else x : h (n-1)) (const []))
                    -- :: Int -> [a]                             --:: Int -> [a]
-- Ejercicio N
drop :: Int -> [a] -> [a]
drop = flip (foldr' (\x h -> \n -> if n==0 then x : h n else h (n-1)) (const []))

-- Ejercicio O
elemAt :: Int -> [a] -> a
-- PRECOND: El elemento esta en la posicion dada.
elemAt = flip (foldr' (\x h -> \n -> if n==0 then x else h (n-1)) (const (error "No existe el elemento en la posicion dada")))

-- Ejercicio 10
-- Ejercicio A
--filter :: (a' -> Bool) -> [a'] -> [a']
--id     :: a -> a
---------------------------------------- (a'<-Bool)
--filter id :: [Bool] -> [Bool]

-- Ejercicio B
--map                   :: (a' -> b') -> [a'] -> [b']
--(\x y z -> (x, y, z)) :: a -> b -> c -> (a,b,c)
--------------------------------------------------------- (a'<-a) (b'<-b -> c -> (a,b,c))
--map (\x y z -> (x, y, z)) :: [a] -> [b -> c -> (a,b,c)]

-- Ejercicio C
--map :: (a' -> b') -> [a'] -> [b']
--(+) :: Int -> Int -> Int
----------------------------------- (a'<-Int) (b'<-Int -> Int)
--map (+) :: [Int] -> [Int -> Int]

-- Ejercicio D
--filter :: (a' -> Bool) -> [a'] -> [a']
--fst    :: (a,b) -> a
---------------------------------------- (a'<-(a,b)) (a<-Bool)
--filter fst :: [(Bool,b)] -> [(Bool,b)]

-- Ejercicio E
--flip  :: (a' -> b' -> c') -> b' -> a' -> c'
--const :: a -> b -> a
--------------------------------------------- (a'<-a) (b'<-b) (c'<-a)
--flip const :: b -> a -> a
--(+)        :: Int -> Int -> Int
--------------------------------- (b <- Int -> Int -> Int)
--flip const (+) :: a -> a

--filter         :: (a' -> Bool) -> [a'] -> [a']
--flip const (+) :: a -> a
------------------------------------------------ (a'<-a) (a<-Bool)
--filter (flip const (+)) :: [Bool] -> [Bool]

-- Ejercicio F
--map   :: (a' -> b') -> [a'] -> [b']
--const :: a -> b -> a
------------------------------------- (a'<-a) (b'<-b->a)
--map const :: [a] -> [b -> a]

-- Ejercicio G
twice f x = f (f x)
--map   :: (a' -> b') -> [a'] -> [b']
--twice :: (a -> a) -> a -> a
------------------------------------- (a'<-a->a) (b'<-a->a)
--map twice :: [a -> a] -> [a -> a]

-- Ejercicio H
--foldr :: (a' -> b' -> b') -> b' -> [a'] -> b'
--twice :: (a -> a) -> a -> a
----------------------------------------------- (a'<-a->a) (b'<-a)
--foldr twice :: a -> [a -> a] -> a

-- Ejercicio I
--zipWith :: (a' -> b' -> c') -> [a'] -> [b'] -> [c']
--fst     :: (a,b) -> a
----------------------------------------------------- (a'<-(a,b)) (a<-b'->c')
--zipWith fst :: [(b' -> c',b)] -> [b'] -> [c']

-- Ejercicio J
--foldr                    :: (a' -> b' -> b') -> b' -> [a'] -> b'
--(\x r z -> (x, z) : r z) :: a -> (b -> [(a,b)]) -> b -> [(a,b)]
------------------------------------------------------------------ (a'<-a) (b'<-b -> [(a,b)])
--foldr (\x r z -> (x, z) : r z) :: (b -> [(a,b)]) -> [a] -> b -> [(a,b)]

--foldr (\x r z -> (x, z) : r z) :: (b' -> [(a',b')]) -> [a'] -> b' -> [(a',b')]
--const []                       :: b -> [a]
-------------------------------------------------------------------------------- (b'<-b) (a<-(a',b'))
--foldr (\x r z -> (x, z) : r z) (const []) :: [a'] -> b -> [(a',b)]


--NOTAS IMPORTANTES
-- NO OLVIDAR LOS TIPOS CUANDO HACER FLIP, YA QUE HAY FUNCIONES QUE SE RETORNAN
-- SI HAY UNA FUNCION RECURSIVA PARCIAL NO OLVIDAR PONER EL CASO BORDE, EJEMPLO LA LISTA CON UN SOLO ELELEMTNO
-- preguntar a fidel lo de abajo
-- NO HACEMOS foldr DOS VECES, HACEMOS RECURSION SOBRE LA SEGUNDA ESTRUCTURA USANDO UN case of
-- POR CONVENCION SOLO EL foldr (EL FOLD SOBRE LISTAS) ES EL QUE EL PRIMER PARAMETRO ES LA FUNCION RECURSIVA, EN TODAS LAS DEMAS LA PRIMERA FUNCION O CONSTANTE ES EL BASE

-- EJERICICIO EXTRAS
type Nombre = Planilla
data Planilla = Fin Int | Registro Nombre Planilla
foldPlanilla :: (Int -> b) -> (Nombre -> b -> b) -> Planilla -> b
foldPlanilla g _ (Fin m)             = g m
foldPlanilla g f (Registro nombre p) = f nombre (foldPlanilla g f p)
-- en arboles es siempre el caso base primero en los tipos
data Dungeon a = Habitacion a | PasajeCon a (Dungeon a) | PasajeSin (Dungeon a)
foldDungeon :: (b -> b) -> (a -> b -> b) -> (a -> b) -> Dungeon a -> b
foldDungeon _ _ h (Habitacion x)  = h x
foldDungeon f g h (PasajeCon x d) = g x (foldDungeon f g h d)
foldDungeon f g h (PasajeSin d)   = f (foldDungeon f g h d)

-- Ejercicio 6
capasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen' _ Prepizza    = []
capasQueCumplen' p (Capa i pz) = if p i
                                  then i : capasQueCumplen' p pz
                                  else capasQueCumplen' p pz

conCapasDe' :: Pizza -> Pizza -> Pizza
conCapasDe' Prepizza    pz  = pz
conCapasDe' (Capa i pz) pz' = Capa i (conCapasDe' pz pz')

-- Ejercicio a
Prop: ¿para todo f. length . capasQueCumplen f = cantidadCapasQueCumplen f?
Dem: por ppio de ext.:
¿para todo f. (length . capasQueCumplen f) p = cantidadCapasQueCumplen f?
por (.)
¿para todo f. para todo p. length (capasQueCumplen f p) = cantidadCapasQueCumplen f p?
sea h y p'. Por ppio de ind. en la estructura de p':
Caso base, p' = Prepizza) ¿length (capasQueCumplen h Prepizza) = cantidadCapasQueCumplen h Prepizza?
Caso ind., p' = Capa i pz) HI) ¡length (capasQueCumplen h pz) = cantidadCapasQueCumplen h pz!
                           TI) ¿length (capasQueCumplen h (Capa i pz)) = cantidadCapasQueCumplen h (Capa i pz)?
Dem caso base:
Lado izq:
length (capasQueCumplen h Prepizza)
= def capasQueCumplen.1
length []
= def length.1
0
Lado der:
cantidadCapasQueCumplen h Prepizza
= def cantidadCapasQueCumplen.1
0   -- VALE EL CASO
Dem caso ind.:
Lado izq:
length (capasQueCumplen h (Capa i pz))
= def cantidadCapasQueCumplen.2
length (if h i then i : capasQueCumplen h pz else capasQueCumplen h pz)
= prop if-then-else
if h i then length (i : capasQueCumplen h pz) else length (capasQueCumplen h pz)
= def length.2
if h i then 1 + length (capasQueCumplen h pz) else length (capasQueCumplen h pz)
= HI, HI
if h i then 1 + cantidadCapasQueCumplen h pz else cantidadCapasQueCumplen h pz
Lado der:
cantidadCapasQueCumplen h (Capa i pz)
= def cantidadCapasQueCumplen.2
unoSi (h i) + cantidadCapasQueCumplen h pz
= Lema if-unoSi
if h i then 1 + cantidadCapasQueCumplen h pz else cantidadCapasQueCumplen h pz   -- VALE EL CASO VALE LA PROPIEDAD

Lema if-unoSi: ¿para todo b. para todo n. if b then 1 + n else n = unoSi b + n?
Dem: sea b' y m:
Caso 1, b' = True) ¿if True then 1 + m else m = unoSi True + m?
Caso 2, b' = False) ¿if False then 1 + m else m = unoSi False + m?
Dem caso 1:
Lado izq:
if True then 1 + m else m
= eval-if
1 + m
Lado der:
unoSi True + m
= def unoSi.1
1 + m   --  VALE EL CASO
Dem caso 2:
Lado izq:
if False then 1 + m else m
= eval-if
m
Lado der:
unoSi False + m
= def unoSi
0 + m
= aritm.
m   -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio b
Prop: ¿para todo f. para todo p1 . para todo p2 . cantidadCapasQueCumplen f (conCapasDe p1 p2) = cantidadCapasQueCumplen f p1 + cantidadCapasQueCumplen f p2?
Dem: sea h p1' y p2'. Por ppio de ind. en la estructura de p1':
Caso base, p1' = Prepizza) ¿cantidadCapasQueCumplen h (conCapasDe Prepizza p2') = cantidadCapasQueCumplen h Prepizza + cantidadCapasQueCumplen h p2'?
Caso ind., p1' = Capa i pz) HI) ¡cantidadCapasQueCumplen h (conCapasDe pz p2') = cantidadCapasQueCumplen h pz + cantidadCapasQueCumplen h p2'!
                            TI) ¿cantidadCapasQueCumplen h (conCapasDe (Capa i pz) p2') = cantidadCapasQueCumplen h (Capa i pz) + cantidadCapasQueCumplen h p2'?
Dem caso base:
Lado izq:
cantidadCapasQueCumplen h (conCapasDe Prepizza p2')
= def conCapasDe.1
cantidadCapasQueCumplen h p2'
Lado der:
cantidadCapasQueCumplen h Prepizza + cantidadCapasQueCumplen h p2'
= def cantidadCapasQueCumplen.1
0 + cantidadCapasQueCumplen h p2'
= aritm.
cantidadCapasQueCumplen h ps2'  -- VALE EL CASO
Dem caso ind.:
Lado izq:
cantidadCapasQueCumplen h (conCapasDe (Capa i pz) p2')
= def conCapasDe.2
cantidadCapasQueCumplen h (Capa i (conCapasDe pz p2'))
= def cantidadCapasQueCumplen.2
unoSi (h i) + cantidadCapasQueCumplen h (conCapasDe pz p2')
Lado der:
cantidadCapasQueCumplen h (Capa i pz) + cantidadCapasQueCumplen h p2'
= def cantidadCapasQueCumplen.2
unoSi (h i) + cantidadCapasQueCumplen h pz + cantidadCapasQueCumplen h p2'
= HI
unoSi (h i) + cantidadCapasQueCumplen h (conCapasDe pz p2')  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio c
Prop: ¿para todo f. para todo p1 . para todo p2 . conCapasTransformadas f (conCapasDe p1 p2) = conCapasDe (conCapasTransformadas f p1) (conCapasTransformadas f p2)?
Dem: sea h p1' p2'. Por ppio de ind. en la estructura de p1':
Caso base, p1' = Prepizza) ¿conCapasTransformadas h (conCapasDe Prepizza p2') = conCapasDe (conCapasTransformadas h Prepizza) (conCapasTransformadas h p2')?
Caso ind., p1' = Capa i pz) HI) ¡conCapasTransformadas h (conCapasDe pz p2') = conCapasDe (conCapasTransformadas h pz) (conCapasTransformadas h p2')!
                            TI) ¿conCapasTransformadas h (conCapasDe (Capa i pz) p2') = conCapasDe (conCapasTransformadas h (Capa i pz)) (conCapasTransformadas h p2')?
Dem caso base:
Lado izq:
conCapasTransformadas h (conCapasDe Prepizza p2')
= def conCapasDe.1
conCapasTransformadas h p2'
Lado der:
conCapasDe (conCapasTransformadas h Prepizza) (conCapasTransformadas h p2')
= def conCapasTransformadas.1
conCapasDe Prepizza (conCapasTransformadas h p2')
= def conCapasDe.1
conCapasTransformadas h p2'  -- VALE EL CASO
Dem caso ind.:
Lado izq:
conCapasTransformadas h (conCapasDe (Capa i pz) p2')
= def conCapasDe.2
conCapasTransformadas h (Capa i (conCapasDe pz p2'))
= def conCapasTransformadas.2
Capa (h i) (conCapasTransformadas h (conCapasDe pz p2'))
Lado der:
conCapasDe (conCapasTransformadas h (Capa i pz)) (conCapasTransformadas h p2')
= def conCapasTransformadas.2
conCapasDe (Capa (h i) (conCapasTransformadas h pz)) (conCapasTransformadas h p2')
= def conCapasDe.2
Capa (h i) (conCapasDe (conCapasTransformadas h pz) (conCapasTransformadas h p2'))
= HI
Capa (h i) (conCapasTransformadas h (conCapasDe pz p2'))  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio d
Prop: ¿para todo f. cantidadCapasQueCumplen f . soloLasCapasQue f = cantidadCapasQueCumplen f?
Dem: por ppio de ext. y por (.)
¿para todo f. para todo p. cantidadCapasQueCumplen f (soloLasCapasQue f p) = cantidadCapasQueCumplen f p?
sea h y p'. Por ppio de ind. en la estructura de p':
Caso base, p' = Prepizza) ¿cantidadCapasQueCumplen h (soloLasCapasQue h Prepizza) = cantidadCapasQueCumplen h Prepizza?
Caso ind., p' = Capa i pz) HI) ¡cantidadCapasQueCumplen h (soloLasCapasQue h pz) = cantidadCapasQueCumplen h pz!
                           TI) ¿cantidadCapasQueCumplen h (soloLasCapasQue h (Capa i pz)) = cantidadCapasQueCumplen h (Capa i pz)?
Dem caso base:
Lado izq:
cantidadCapasQueCumplen h (soloLasCapasQue h Prepizza)
= dem soloLasCapasQue.1
cantidadCapasQueCumplen h Prepizza  -- VALE EL CASO
Dem caso ind.:
Lado izq:
cantidadCapasQueCumplen h (soloLasCapasQue h (Capa i pz))
= def soloLasCapasQue.2
cantidadCapasQueCumplen h (if h i then Capa i (soloLasCapasQue h pz) else soloLasCapasQue h pz)
= prop if-then-else
if h i then cantidadCapasQueCumplen h (Capa i (soloLasCapasQue h pz)) else cantidadCapasQueCumplen h (soloLasCapasQue h pz)
= def cantidadCapasQueCumplen.2
if h i then unoSi (h i) + cantidadCapasQueCumplen h (soloLasCapasQue h pz) else cantidadCapasQueCumplen h (soloLasCapasQue h pz)
= HI, HI
if h i then unoSi (h i) + cantidadCapasQueCumplen h pz else cantidadCapasQueCumplen h pz
Lado der:
cantidadCapasQueCumplen h (Capa i pz)
= def cantidadCapasQueCumplen.2
unoSi (h i) + cantidadCapasQueCumplen h pz
= Lema if-then-unoSi
if h i then unoSi (h i) + cantidadCapasQueCumplen h pz else cantidadCapasQueCumplen h pz  -- VALE EL CASO VALE LA PROPIEDAD

Lema if-then-unoSi: ¿para todo b. para todo n. if b then unoSi b + n else n = unoSi b + n?
Dem: sea b' y m:
Caso 1, b' = True) ¿if True then unoSi True + m else m = unoSi True + m?
Caso 2, b' = False) ¿if False then unoSi False + m else m = unoSi False + m?
Dem caso 1:
Lado izq:
if True then unoSi True + m else m
= eval-if
unoSi True + m  -- VALE EL CASO
Dem caso ind.:
Lado izq:
if False then unoSi False + m else m
= eval-if
m
Lado der:
unoSi False + m
= def unoSi.2
0 + m
= aritm.
m  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio 8
sum :: [Int] -> Int
sum []     = 0
sum (n:ns) = n + sum ns

suma' :: (Int, Int) -> Int
suma' (x, y) = x+y

many :: Int -> (a -> a) -> a -> a
many 0 f = id
many n f = f . many (n - 1) f

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- Ejercicio a
Prop: ¿map id = id?
Dem: por ppio de ext. y por id
¿para todo xs. map id xs = xs?
sea zs. Por ppio de ind. en la estructura de zs:
Caso base, zs = []) ¿map id [] = []?
Caso ind., zs = x:xs') HI) ¡map id xs' = xs'!
                       TI) ¿map id (x:xs') = x:xs'?
Dem caso base:
Lado izq:
map id []
= def map.1
[]  -- VALE EL CASO
Dem caso ind.:
Lado izq:
map id (x:xs')
= def map.2
id x : map id xs'
= def id, HI
x : xs'  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio b
Prop: ¿para todo f. para todo g. map f . map g = map (f . g)?
Dem: por ppio de ext. y por (.)
¿para todo f. para todo g. para todo xs. map f (map g xs) = map (f . g) xs?
sea h h' y zs. Por ppio de ind. en la estructura de zs:
Caso base, zs = []) ¿map f (map g []) = map (f . g) []?
Caso ind., zs = x:xs') HI) ¡map h (map h' xs') = map (h . h') xs'!
                       TI) ¿map h (map h' (x:xs')) = map (h . h') (x:xs')?
Dem caso base:
Lado izq:
map f (map g [])
= def map.1
map f []
= def map.1
[]
Lado der:
map (f . g) []
= def map.1
[]  -- VALE EL CASO
Dem caso ind.:
Lado izq:
map h (map h' (x:xs'))
= def map.2
map h (h' x : map h' xs')
= def map.2
h (h' x) : map h (map h' xs')
= Lema map-compose
Lado der:
map (h . h') (x:xs')
= def map.2
(h . h') x : map (h . h') xs'
= def (.), HI
h (h' x) : map h (map h' xs')  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio c
Prop: ¿para todo f. para todo xs. para todo ys. map f (xs ++ ys) = map f xs ++ map f ys?
Dem: sea h zs y ws. Por ppio de ind. en la estructura de zs:
Caso base, zs = []) ¿map h ([] ++ ws) = map h [] ++ map h ws?
Caso ind., zs = x:xs') HI) ¡map h (xs' ++ ws) = map h xs' ++ map h ws!
                       TI) ¿map h ((x:xs') ++ ws) = map h (x:xs') ++ map h ws?
Dem caso base:
Lado izq:
map h ([] ++ ws)
= def (++).1
map h ws
Lado der:
map h [] ++ map h ws
= def map.1
[] ++ map h ws
= def (++).1
map h ws  -- VALE EL CASO
Dem caso ind.:
Lado izq:
map h ((x:xs') ++ ws)
= def (++).2
map h (x : xs' ++ ws)
= def map.2
h x : map h (xs' ++ ws)
Lado der:
map h (x:xs') ++ map h ws
= def map.2
(h x : map h xs') ++ map h ws   -- IMPORTANTE notar el PM del cons
= def (++).2
h x : map h xs' ++ map h ws
= HI
h x : map h (xs' ++ ws)  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio d
Prop: ¿para todo f. concat . map (map f) = map f . concat?
Dem: por ppio de ext. y por (.)
¿para todo f. para todo xss. concat (map (map f) xss) = map f (concat xss)?
sea h y zss. Por ppio de ind. en la estructura de zss:
Caso base, zss = []) ¿concat (map (map h) []) = map h (concat [])?
Caso ind., zss = xs:xss') HI) ¡concat (map (map h) xss') = map h (concat xss')!
                          TI) ¿concat (map (map h) (xs:xss')) = map h (concat (xs:xss'))?
Dem caso base:
Lado izq:
concat (map (map h) [])
= def map.1
concat []
= def concat.1
[]
Lado der:
map h (concat [])
= def concat.1
map h []
= def map.1
[]  -- VALE EL CASO
Dem caso ind:
Lado izq:
concat (map (map h) (xs:xss'))
= def map.2
concat (map h xs : map (map h) xss')
= def concat.2
map h xs ++ concat (map (map h) xss')
= HI
map h xs ++ map h (concat xss')
Lado der:
map h (concat (xs:xss'))
= def concat.2
map h (xs ++ concat xss')
= Lema prop. map f (xs ++ ys) = map f xs ++ map f ys
map h xs ++ map h (concat xss')  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio e
Prop: ¿foldr ((+) . suma') 0 = sum . map suma'?
Dem: por ppio de ext. y por (.)
¿para todo ns. foldr ((+) . suma') 0 ns = sum (map suma' ns)?
sea ms. Por ppio de ind. en la estructura de ms:
Caso base, ms = []) ¿foldr ((+) . suma') 0 [] = sum (map suma' [])?
Caso ind., ms = n:ns') HI) ¡foldr ((+) . suma') 0 ns' = sum (map suma' ns')!
                       TI) ¿foldr ((+) . suma') 0 (n:ns') = sum (map suma' (n:ns'))?
Dem caso base:
Lado izq:
foldr ((+) . suma') 0 []
= def foldr.1
0
Lado der:
sum (map suma' [])
= def map.1
sum []
= def sum.1
[]  -- VALE EL CASO
Dem caso ind.:
foldr ((+) . suma') 0 (n:ns')
= def foldr.2
((+) . suma') n (foldr ((+) . suma') 0 ns')
= def (.), HI
(+) (suma' n) (sum (map suma' ns'))
= seccion de operadores
suma' n + sum (map suma' ns')
Lado der:
sum (map suma' (n:ns'))
= def map.2
sum (suma' n : map suma' ns')
= def sum.2
suma' n + sum (map suma' ns')  -- VALE EL CASO VALE LA PROPIEDAD.

-- Ejercicio f
Prop: ¿para todo f. para todo z. foldr f z . foldr (:) [] = foldr f z?
Dem: por ppio de ext. y por (.):
¿para todo f. para todo z. para todo xs. foldr f z (foldr (:) [] xs) = foldr f z xs?
sea h z' zs. Por ppio de ind. en la estructura de zs:
Caso base, zs = []) ¿foldr h z' (foldr (:) [] []) = foldr h z' []?
Caso ind., zs = x:xs') HI) ¡foldr h z' (foldr (:) [] xs') = foldr h z' xs'!
                       TI) ¿foldr h z' (foldr (:) [] (x:xs')) = foldr h z' (x:xs')?
Dem caso base:
foldr h z' (foldr (:) [] [])
= def foldr.1
foldr h z' []  -- VALE EL CASO
Dem caso ind.:
Lado izq:
foldr h z' (foldr (:) [] (x:xs'))
= def foldr.2
foldr h z' ((:) x (foldr (:) [] xs'))
= seccion de operadores
foldr h z' (x : (foldr (:) [] xs'))
= def foldr.2
h x (foldr h z' (foldr (:) [] xs'))
= HI
h x (foldr h z' xs')
Lado der:
foldr h z' (x:xs')
= def foldr.2
h x (foldr h z' xs')  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio g
Prop: ¿para todo f. para todo z. para todo xs. para todo ys. foldr f z (xs ++ ys) = foldr f (foldr f z ys) xs?
Dem: sea h z' zs y ws. Por ppio de ind. en la estructura de zs:
Caso base, zs = []) ¿foldr h z' ([] ++ ws) = foldr h (foldr h z' ws) []?
Caso ind., zs = x:xs') HI) ¡foldr h z' (xs' ++ ws) = foldr h (foldr h z' ws) xs'!
                       TI) ¿foldr h z' ((x:xs') ++ ws) = foldr h (foldr h z' ws) (x:xs')?
Dem caso base:
Lado izq:
foldr h z' ([] ++ ws)
= def (++).1
foldr h z' ws
Lado der:
foldr h (foldr h z' ws) []
= def foldr.1
foldr h z' ws  -- VALE EL CASO
Dem caso ind.:
foldr h z' ((x:xs') ++ ws)
= def (++).2
foldr h z' (x : xs' ++ ws)
= def foldr.2
h x (foldr h z' (xs' ++ ws))
Lado der:
foldr h (foldr h z' ws) (x:xs')
= def foldr.2
h x (foldr h (foldr h z' ws) xs')
= HI
h x (foldr h z' (xs' ++ ws))  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio h
Prop: ¿(+1) . foldr (+) 0 = foldr (+) 1?
Dem: por ppio de ext. y por (.)
¿para todo ns. (+1) (foldr (+) 0 ns) = foldr (+) 1 ns?
sea ms. Por ppio de ind. en la estructura de ms:
Caso base, ms = []) ¿(+1) (foldr (+) 0 []) = foldr (+) 1 []?
Caso ind., ms = n:ns') HI) ¡(+1) (foldr (+) 0 ns') = foldr (+) 1 ns'!
                       TI) ¿(+1) (foldr (+) 0 (n:ns')) = foldr (+) 1 (n:ns')?
Dem caso base:
Lado izq:
(+1) (foldr (+) 0 [])
= def foldr.1
(+1) 0
= seccion de operadores
1 + 0
= airtm.
1
Lado der:
foldr (+) 1 []
= def foldr.1
1  -- VALE EL CASO
Dem caso ind:
(+1) (foldr (+) 0 (n:ns'))
= def foldr.2
(+1) ((+) n (foldr (+) 0 ns'))
= seccion de operadores
1 + (n + (foldr (+) 0 ns'))
= aritm.
1 + n + (foldr (+) 0 ns')
= aritm.
n + 1 + (foldr (+) 0 ns')
= seccion de operadores
n + (+1) (foldr (+) 0 ns')
= HI
n + foldr (+) 1 ns'
Lado der:
foldr (+) 1 (n:ns')
= def foldr.2
(+) n (foldr (+) 1 ns')
= seccion de operadores
n + foldr (+) 1 ns'  -- VALE EL CAOS VALE LA PROPIEDAD

-- Ejercicio i
Prop: ¿para todo n. para todo f. many n f = foldr (.) id (replicate n f)?
Dem: sea m y h. Por ppio de ind. en la estructura de m:
CB m = 0) ¿many 0 h = foldr (.) id (replicate 0 h)?
CI m /= 0) HI) ¡many (m-1) h = foldr (.) id (replicate (m-1) h)!
           TI) ¿many m h = foldr (.) id (replicate m h)?
Dem CB:
Lado izq:
many 0 h
= def many.1
id
Lado der:
foldr (.) id (replicate 0 h)
= def replicate.1
foldr (.) id []
=def foldr.1
id  -- VALE EL CASO
Dem CI:
Lado izq:
many m h
= def many.2
h . many (m-1) h
Lado der:
foldr (.) id (replicate m h)
= def replicate.2
foldr (.) id (h : replicate (m-1))
= foldr.2
(.) h (foldr (.) id (replicate (m-1) h))
= seccion de operadores
h . foldr (.) id (replicate (m-1) h)
= HI
h . many (m-1) h  -- VALE EL CASO VALE LA PROPIEDAD

-- Ejercicio j
Prop: ¿para todo f. para todo xs. para todo ys. zipWith (flip f) xs ys = map (uncurry f) (flip zip xs ys)?
Dem: sea h zs y ws. Por ppio de ind. en la estructura de zs:
CB zs = []) ¿zipWith (flip h) [] ws = map (uncurry h) (flip zip [] ws)?
CI zs = x:xs')
 CI.1 ws = []) ¿zipWith (flip h) (x:xs') [] = map (uncurry h) (flip zip (x:xs') [])?
 CI.2 ws = y:ys') HI) ¡zipWith (flip h) xs' ys' = map (uncurry h) (flip zip xs' ys')!
                  TI) ¿zipWith (flip h) (x:xs') (y:ys') = map (uncurry h) (flip zip (x:xs') (y:ys'))?
Dem CB:
Lado izq:
zipWith (flip h) [] ws
= def zipWith.1
[]
Lado der:
map (uncurry h) (flip zip [] ws)
= def flip
map (uncurry h) (zip ws [])
= def zip.2
map (uncurry h) []
= def map.1
[]  -- VALE EL CASO
Dem CI.1:
Lado izq:
zipWith (flip h) (x:xs') []
= def zipWith.2
[]
Lado der:
map (uncurry h) (flip zip (x:xs') [])
= def flip
map (uncurry h) (zip [] (x:xs'))
= def zip.1
map (uncurry h) []
= def map.1
[]  -- VALE EL CASO
Dem CI.2:
Lado izq:
zipWith (flip h) (x:xs') (y:ys')
= def zipWith.3
flip h x y : zipWith (flip h) xs' ys'
= def flip
h y x : zipWith (flip h) xs' ys'
= HI
h y x : map (uncurry h) (flip zip xs' ys')
= def flip
h y x : map (uncurry h) (zip ys' xs')
Lado der:
map (uncurry h) (flip zip (x:xs') (y:ys'))
= def flip
map (uncurry h) (zip (y:ys') (x:xs'))
= def zip.3
map (uncurry h) ((y,x) : zip ys' xs')
= def map.2
uncurry h (y,x) : map (uncurry h) (zip ys' xs')
= def uncurry
h y x : map (uncurry h) (zip ys' xs')  -- VALE EL CASO PARA VALE LA PROPIEDAD

-- Ejercicio k
Prop: ¿para todo f. para todo g. para todo h. para todo z.
       si (para todo x. para todo y. h (f x y) = g x (h y))
       entonces h . foldr f z = foldr g (h z)?
Dem: por ppio de ext. y por (.)
¿para todo f. para todo g. para todo h. para todo z. para todo xs.
 si para todo x. para todo y. h (f x y) = g x (h y)
 entonces h (foldr f z xs) = foldr g (h z) xs?
sean f' g' h' z' zs. Por ppio de ind. en la estructura de zs:
CB zs = []) ¿si (para todo x. para todo y. h' (f' x y) = g' x (h' y))
                     entonces h' (foldr f' z' []) = foldr g' (h' z') []?
CI zs = x':xs') HI) ¡si (para todo x. para todo y. h' (f' x y) = g' x (h' y))
                    entonces h' (foldr f' z' xs') = foldr g' (h' z') xs'!
                TI) ¿si (para todo x. para todo y. h' (f' x y) = g' x (h' y))
                    entonces h' (foldr f' z' (x':xs')) = foldr g' (h' z') (x':xs')?
Dem CB:
Lado izq:
h' (foldr f' z' [])
= def foldr.1
h' z'
Lado der:
foldr g' (h' z') []
= def foldr.1
h' z'  -- VALE EL CASO
Dem CI:
Lado izq:
h' (foldr f' z' (x':xs'))
= def foldr.2
h' (f' x' (foldr f' z' xs'))
=          (Hipotesis de la propiedad, con x = x' e y = foldr f' z' xs')
g' x' (h' (foldr f' z' xs'))
Lado der:
foldr g' (h' z') (x':xs')
= def foldr.2
g' x' (foldr g' (h' z') xs')
= HI
g' x' (h' (foldr f' z' xs'))  -- VALE EL CAOS VALE LA PROPIEDAD
