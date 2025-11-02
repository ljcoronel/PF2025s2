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
sinLactosa pz = soloLasCapasQue (not . esQueso) pz

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False

-- Ejercicio B
aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa pz = cantidadCapasQueCumplen esQueso pz == 0

-- Ejercicio C
cantidadDeQueso :: Pizza -> Int
cantidadDeQueso pz = cantidadCapasQueCumplen esQueso pz

-- Ejercicio D
conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas pz = conCapasTransformadas duplicarAceitunas pz

duplicarAceitunas :: Ingrediente -> Ingrediente
duplicarAceitunas (Aceitunas n) = Aceitunas (n*2)
duplicarAceitunas i             = i

-- Ejercicio 3
pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada f z Prepizza    = z
pizzaProcesada f z (Capa i pz) = f i (pizzaProcesada f z pz)

-- Ejercicio 4
cantidadCapasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen' p = pizzaProcesada (\i n -> unoSi (p i) + n) 0

conCapasTransformadas' :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas' f = pizzaProcesada (\i pz -> Capa (f i) pz) Prepizza
                                          -- (\i -> Capa (f i))
                                          -- (\i -> (Capa . f) i)
                                          -- (Capa . f)

soloLasCapasQue' :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' p = pizzaProcesada (\i pz -> if p i then Capa i pz else pz) Prepizza

sinLactosa' :: Pizza -> Pizza
sinLactosa' = pizzaProcesada (\i pz -> if (not . esQueso) i then Capa i pz else pz) Prepizza

aptaIntolerantesLactosa' :: Pizza -> Bool
aptaIntolerantesLactosa' = pizzaProcesada (\i b -> (not . esQueso) i && b) True

cantidadDeQueso' :: Pizza -> Int
cantidadDeQueso' = pizzaProcesada (\i n -> unoSi (esQueso i) + n) 0

conElDobleDeAceitunas' :: Pizza -> Pizza
conElDobleDeAceitunas' = pizzaProcesada (\i pz -> Capa (duplicarAceitunas i) pz) Prepizza

-- Ejercicio 5
-- Ejercicio A
cantidadAceitunas :: Pizza -> Int
cantidadAceitunas = pizzaProcesada (\i n -> aceitunas i + n) 0

aceitunas :: Ingrediente -> Int
aceitunas (Aceitunas n) = n
aceitunas _             = 0

-- Ejercicio B
capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen p = pizzaProcesada (\i is -> if p i then i : is else is) []

-- Ejercicio C
conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada = pizzaProcesada (\i pz -> juntarAceitunas i pz) Prepizza

juntarAceitunas :: Ingrediente -> Pizza -> Pizza
juntarAceitunas (Aceitunas n) (Capa (Aceitunas m) pz) = Capa (Aceitunas (n+m)) pz
juntarAceitunas i             pz                      = Capa i pz

-- Ejercicio D
conCapasDe :: Pizza -> Pizza -> Pizza
conCapasDe = pizzaProcesada (\i h -> \pz -> Capa i (h pz)) id

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
sum = foldr' (\n m -> n + m) 0

-- Ejercicio B
length :: [a] -> Int
length = foldr' (\_ n -> 1 + n) 0

-- Ejercicio C
map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr' (\x ys -> (f x) : ys) []

-- Ejercicio D
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr' (\x xs -> if p x then x : xs else xs) []

-- Ejercicio E
find :: (a -> Bool) -> [a] -> Maybe a
find p = foldr' (\x m -> if p x then Just x else m) Nothing

-- Ejercicio F
any :: (a -> Bool) -> [a] -> Bool
any p = foldr' (\x b -> p x || b) False

-- Ejercicio G
all :: (a -> Bool) -> [a] -> Bool
all p = foldr' (\x b -> p x && b) True

-- Ejercicio H
countBy :: (a -> Bool) -> [a] -> Int
countBy p = foldr' (\x n -> unoSi (p x) + n) 0

-- Ejercicio I
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p = foldr' (\x (xs,ys) -> if p x then (x:xs,ys) else (xs,x:ys)) ([],[])

-- Ejercicio J
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f = foldr' (\x h -> \ys -> case ys of [] -> []; (y:ys) -> f x y : h ys) (const [])

-- Ejercicio K
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f e = foldr' (\x ys -> f x e : ys) []

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























--NOTAS IMPORTANTES
-- NO OLVIDAR LOS TIPOS CUANDO HACER FLIP, YA QUE HAY FUNCIONES QUE SE RETORNAN
-- SI HAY UNA FUNCION RECURSIVA PARCIAL NO OLVIDAR PONER EL CASO BORDE, EJEMPLO LA LISTA CON UN SOLO ELELEMTNO
-- preguntar a fidel lo de abajo
-- NO HACEMOS foldr DOS VECES, HACEMOS RECURSION SOBRE LA SEGUNDA ESTRUCTURA USANDO UN case of
-- POR CONVENCION SOLO EL foldr (EL FOLD SOBRE LISTAS) ES EL QUE EL PRIMER PARAMETRO ES LA FUNCION RECURSIVA, EN TODAS LAS DEMAS LA PRIMERA FUNCION O CONSTANTE ES EL BASE
--

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
