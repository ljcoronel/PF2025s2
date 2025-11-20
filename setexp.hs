data SetExp a = EmptyS
              | Singleton a
              | Union (SetExp a) (SetExp a)
              | Intersect (SetExp a) (SetExp a)
              | Diff (SetExp a) (SetExp a) -- resta
                -- [1,2,3] - [2,3] = [1]
     deriving Show

-- la lista resultante no tiene elementos repetidos
-- Ejercicio 1
evalSL :: Eq a => SetExp a -> [a]
evalSL EmptyS            = []
evalSL (Singleton x)     = [x]
evalSL (Union s1 s2)     = sinRepetidos (evalSL s1 ++ evalSL s2)
evalSL (Intersect s1 s2) = intersectL (evalSL s1) (evalSL s2)
evalSL (Diff s1 s2)      = diffL (evalSL s1) (evalSL s2)

intersectL :: Eq a => [a] -> [a] -> [a]
intersectL []     _  = []
intersectL (x:xs) ys = if elem x ys then x : intersectL xs ys else intersectL xs ys

diffL :: Eq a => [a] -> [a] -> [a]
diffL []     _  = []
diffL (x:xs) ys = if elem x ys then diffL xs ys else x : diffL xs ys

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos []     = []
sinRepetidos (x:xs) = if elem x (sinRepetidos xs) then sinRepetidos xs else x : sinRepetidos xs

-- Ejercicio 2
evalSF :: Eq a => SetExp a -> a -> Bool
evalSF EmptyS            = const False
evalSF (Singleton x)     = (==x)
evalSF (Union s1 s2)     = \x -> evalSF s1 x || evalSF s2 x
evalSF (Intersect s1 s2) = \x -> evalSF s1 x && evalSF s2 x
evalSF (Diff s1 s2)      = \x -> evalSF s1 x && not (evalSF s2 x)

-- vale 

-- por ejemplo:
-- elem e [1,2,3] = (\x -> x == 1 || x == 2 || x == 3) e

-- Propiedad:
-- Para todo e, s. 
-- elem e (evalSL s) = (evalSF s) e
-- Tarea: demostrar la propiedad

-- importante, no evaluar la expresión
-- Ejercicio 3
simpSE :: SetExp a -> SetExp a
simpSE EmptyS            = EmptyS
simpSE (Singleton x)     = Singleton x
simpSE (Union s1 s2)     = cfUnion (simpSE s1) (simpSE s2)
simpSE (Intersect s1 s2) = cfIntersect (simpSE s1) (simpSE s2)
simpSE (Diff s1 s2)      = cfDiff (simpSE s1) (simpSE s2)

cfUnion :: SetExp a -> SetExp a -> SetExp a
cfUnion EmptyS s      = s
cfUnion s      EmptyS = s
cfUnion s1     s2     = Union s1 s2

cfIntersect :: SetExp a -> SetExp a -> SetExp a
cfIntersect EmptyS _      = EmptyS
cfIntersect _      EmptyS = EmptyS
cfIntersect s1     s2     = Intersect s1 s2

cfDiff :: SetExp a -> SetExp a -> SetExp a
cfDiff EmptyS _      = EmptyS
cfDiff s      EmptyS = s
cfDiff s1     s2     = Diff s1 s2

-- Propiedades de simplificación:

-- En la union cuando un conjunto es vacio el resultado
-- es el otro conjunto

-- En la interseccion cuando un conjunto es vacio
-- el resultado es vacio

-- En la diferencia entre conjuntos, restar un conjunto
-- vacio, da el otro conjunto. Y restarle elementos
-- a un conjunto vacio, da vacio.

-- Propiedad:
-- Para s. 
-- evalSF s = evalSF (simpSE s)
-- Tarea: demostrar la propiedad

-- Propiedad:
-- Para s. 
-- evalSL s = evalSL (simpSE s)
-- Tarea: demostrar la propiedad

-- toma los elementos de la lista
-- y una función que dice si van a estar
-- en el conjunto
-- y arma una expresión de conjuntos
-- que representa a dicha función
-- Ejercicio 4
toSetExp :: [a] -> (a -> Bool) -> SetExp a
toSetExp []     _ = EmptyS
toSetExp (x:xs) p = if p x then Union (Singleton x) (toSetExp xs p) else toSetExp xs p

-- Demo:
-- Para todo xs, s
-- evalSF (toSetExp xs (evalSF s)) = evalSF s

DEMO 1
Prop: ¿para todo e. para todo s. elem e (evalSL s) = evalSF s e?
Dem: sea e' y s'. Por ppio de ind. en la estructura de s':
CB.1 s' = EmptyS) ¿elem e' (evalSL EmptyS) = evalSF EmptyS e'?
CB.2 s' = Singleton x) ¿elem e' (evalSL (Singleton x)) = evalSF (Singleton x) e'?
CI.1 s' = Union s1 s2) HI11) ¡elem e' (evalSL s1) = evalSF s1 e'!
                       HI21) ¡elem e' (evalSL s2) = evalSF s2 e'!
                       TI1) ¿elem e' (evalSL (Union s1 s2)) = evalSF (Union s1 s2) e'?
CI.2 s' = Intersect s1 s2) HI12) ¡elem e' (evalSL s1) = evalSF s1 e'!
                           HI22) ¡elem e' (evalSL s2) = evalSF s2 e'!
                           TI2) ¿elem e' (evalSL (Intersect s1 s2)) = evalSF (Intersect s1 s2) e'?
CI.3 s' = Diff s1 s2) HI13) ¡elem e' (evalSL s1) = evalSF s1 e'!
                      HI23) ¡elem e' (evalSL s2) = evalSF s2 e'!
                      TI3) ¿elem e' (evalSL (Diff s1 s2)) = evalSF (Diff s1 s2) e'?
Dem CB.1:
Lado izq:
elem e' (evalSL EmptyS)
= def evalSL.1
elem e' []
= def elem.1
False
Lado der:
evalSF EmptyS e'
= def evalSF.1
const False e'
= def const
False  -- VALE EL CASO
Dem CB.2:
Lado izq:
elem e' (evalSL (Singleton x))
= def evalSL.2
elem e' [x]
= def elem.2
e'==x || elem e' []
= def elem.1
e'==x || False
= Lema conmutatividad-or
False || e'==x
= def ||.2
e'==x
Lado der:
evalSF (Singleton x) e'
= def evalSF.2
(==x) e'
= seccion de operadores
e'==x  -- VALE EL CASO
Dem CI.1:
Lado izq:
elem e' (evalSL (Union s1 s2))
= def evalSL.3
elem e' (sinRepetidos (evalSL s1 ++ evalSL s2))
= Lema elem-sinRepetidos
elem e' (evalSL s1 ++ evalSL s2)
= Lema elem-append
elem e' (evalSL s1) || elem e' (evalSL s2)
= HI11 HI21
evalSF s1 e' || evalSF s2 e'
Lado der:
evalSF (Union s1 s2) e'
= def evalSF.3
(\x -> evalSF s1 x || evalSF s2 x) e'
= beta
evalSF s1 e' || evalSF s2 e'  -- VALE EL CASO
Dem CI.2:
Lado izq:
elem e' (evalSL (Intersect s1 s2))
= def evalSL.4
elem e' (intersectL (evalSL s1) (evalSL s2))
= Lema elem-intersectL
elem e' (evalSL s1) && elem e' (evalSL s2)
= HI12, HI22
evalSF s1 e' && evalSF s2 e'
Lado der:
evalSF (Intersect s1 s2) e'
= def evalSF.4
(\x -> evalSF s1 x && evalSF s2 x) e'
= beta
evalSF s1 e' && evalSF s2 e'  -- VALE EL CASO
Dem CI.3:
Lado izq:
elem e' (evalSL (Diff s1 s2))
= def evalSL.5
elem e' (diffL (evalSL s1) (evalSL s2))
= Lema elem-diffL
elem e' (evalSL s1) && not (elem e' (evalSL s2))
= HI13, HI23
evalSF s1 e' && not (evalSF s2 e')
Lado der:
evalSF (Diff s1 s2) e'
= def evalSF.5
(\x -> evalSF s1 x && not (evalSF s2 x)) e'
= beta
evalSF s1 e' && not (evalSF s2 e')  -- VALE EL CASO VALE LA PROPIEDAD

Lema elem-sinRepetidos: ¿para todo x. para todo xs. elem x (sinRepetidos xs) = elem x xs?  -- consultar lemas
Dem: sean e zs. Por ppio de ind. en la estructura de zs:
CB zs = []) ¿elem e (sinRepetidos []) = elem e []?
CI zs = x':xs') HI) ¡elem e (sinRepetidos xs') = elem e xs'!
                TI) ¿elem e (sinRepetidos (x':xs')) = elem e (x':xs')?
Dem CB:
Lado izq:
elem e (sinRepetidos [])
= def sinRepetidos.1
elem e []  -- VALE EL CASO
Dem CI:
elem e (sinRepetidos (x':xs'))
= def sinRepetidos.2
elem e (if elem x' (sinRepetidos xs') then sinRepetidos xs' else x' : sinRepetidos xs')
= prop of-then-else
if elem x' (sinRepetidos xs') then elem e (sinRepetidos xs') else elem e (x' : sinRepetidos xs')
= def elem.2
if elem x' (sinRepetidos xs') then elem e (sinRepetidos xs') else x'==e || elem e (sinRepetidos xs')
= Lema elem-if-else
x'==e || elem e (sinRepetidos xs')
= HI
x'==e || elem e xs'
Lado der:
elem e (x':xs')
= def elem.2
x'==e || elem e xs'  -- VALE EL CASO VALE LA PROPIEDAD

Lema elem-if-else: ¿para todo x. para todo y. if elem x xs then elem y xs else x==y || elem y xs = x==y || elem y xs?
Dem : sean e e' zs:
C1 e = e') ¿if elem e zs then elem e' zs else True || elem e' zs = True || elem e' zs?
C2 e /= e') ¿if elem e zs then elem e' zs else False || elem e' zs = False || elem e' zs?
Dem C1:
  if elem e zs then elem e' zs else True || elem e' zs
=                                       ((||).1)
  if elem e zs then elem e' zs else True
=                                       (Hipotesis)
  if elem e zs then elem e zs else True
=                                       (Lema else-true)
  True
Lado der:
  True || elem e' zs
=                                       ((||).1)
  True  -- VALE EL CASO
Dem C2:
Lado izq:
  if elem e zs then elem e' zs else False || elem e' zs
=                                       ((||).2)
  if elem e zs then elem e' zs else elem e' zs
=                                       (Lema simplificacion-if)
  elem e' zs
Lado der:
  False || elem e' zs
=                                       ((||).2)
  elem e' zs  -- VALE EL CASO VALE LA PROPIEDAD

Lema else-true: ¿para todo b. si b no es bottom entonces if b then b else True = True?
Dem: sea b':
C1 b' = True) ¿if True then True else True = True?
C2 b' = False) ¿if False then False else True = True?
Dem C1:
  if True then True else True
=                            (eval-if)
  True  -- VALE EL CASO
Dem C2:
Lado izq:
  if False then False else True
=                            (eval-if)
  True  -- VALE EL CASO VALE LA PROPIEDAD

Lema simplificacion-if: ¿para todo b1. para todo b2. si b1 no es bottom entonces if b1 then b2 else b2 = b2?
Dem: sean b1' b2':
C1 b1' = True) ¿if True then b2' else b2' = b2'?
C2 b1' = False) ¿if False then b2' else b2' = b2'?
Dem C1:
  if True then b2' else b2'
=                          (eval-if)
  b2'  -- VALE EL CASO
Dem C2:
Lado izq
  if False then b2' else b2'
=                          (eval-if)
  b2'  -- VALE EL CASO VALE LA PROPIEDAD

Lema elem-append: ¿para todo x. para todo xs. para todo ys. elem x (xs ++ ys) = elem x xs || elem x ys?
Dem: sean e zs ws. Por ppio de ind. en la estructura de zs:
CB zs = []) ¿elem e ([] ++ ws) = elem e [] || elem e ws?
CI zs = x':xs') HI) ¡elem e (xs' ++ ws) = elem e xs' || elem e ws!
                TI) ¿elem e ((x':xs') ++ ws) = elem e (x':xs') || elem e ws?
Dem CB:
Lado izq:
elem e ([] ++ ws)
= def ++.1
elem e ws
Lado der:
elem e [] || elem e ws
= def elem.1
False || elem e ws
= ||.2
elem e ws  -- VALE EL CASO
Dem CI:
Lado izq:
elem e ((x':xs') ++ ws)
= def ++.2
elem e (x' : xs' ++ ws)
= def elem.2
x'==e || elem e (xs' ++ ws)
Lado der:
elem e (x':xs') || elem e ws
= def elem.2
x'==e || elem e xs' || elem e ws
= HI
x'==e || elem e (xs' ++ ws)  -- VALE EL CASO VALE LA PROPIEDAD

Lema elem-intersectL: ¿para todo x. para todo xs. para todo ys. elem x (intersectL xs ys) = elem x xs && elem x ys?
Dem: sean e zs ws. Por ppio de ind. en la estructura de zs:
CB zs = []) ¿elem e (intersectL [] ws) = elem e [] && elem e ws?
CI zs = x':xs') HI) ¡elem e (intersectL xs' ws) = elem e xs' && elem e ws!
                TI) ¿elem e (intersectL (x':xs') ws) = elem e (x':xs') && elem e ws?
Dem CB:
Lado izq:
elem e (intersectL [] ws)
= intersectL.1
elem e []
= def elem.1
False
Lado der:
elem e (x':xs') && elem e []
= def elem.2, elem.1
(x'==e || elem e xs') && False
= Lema conmutatividad-and
False && (x'==e || elem e xs')
= def &&.1
False  -- VALE EL CASO
Dem CI:
Lado izq:
elem e (intersectL (x':xs') ws)
= def intersectL.3
elem e (if elem x' ws then x' : intersectL xs' ws else intersectL xs' ws)
= prop if-then-else
if elem x' ws then elem e (x' : intersectL xs' ws) else elem e (intersectL xs' ws)
= def elem.2
if elem x' ws then x'==e || elem e (intersectL xs' ws) else elem e (intersectL xs' ws)
= HI, HI
if elem x' ws then x'==e || elem e xs' && elem e ws else elem e xs' && elem e ws
= Lema elem-if-then
(x'==e || elem e xs') && elem e ws
Lado der:
elem e (x':xs') && elem e ws
= def elem.2
(x'==e || elem e xs') && elem e ws  -- VALE EL CASO VALE LA PROPIEDAD

Lema elem-if-then: ¿para todo x. para todo y. para todo xs. para todo ys.
                   if elem x xs then x==y || elem y ys && elem y xs else elem y ys && elem y xs = (x==y || elem y ys) && elem y xs?
Dem: sean e e' zs ws:
C1 e==e') ¿if elem e zs then e==e' || elem e' ws && elem e' zs else elem e' ws && elem e' zs = (e==e' || elem e' ws) && elem e' zs?
C2 e/=e') ¿if elem e zs then e==e' || elem e' ws && elem e' zs else elem e' ws && elem e' zs = (e==e' || elem e' ws) && elem e' zs?
Dem C1:
Lado izq:
if elem e zs then e==e' || elem e' ws && elem e' zs else elem e' ws && elem e' zs
= def ==
if elem e zs then True || elem e' ws && elem e' zs else elem e' ws && elem e' zs
= def ||.1
if elem e zs then True else elem e' ws && elem e' zs
= Hipotesis
if elem e zs then True else elem e' ws && elem e zs
= Lema if-else-and
elem e zs
Lado der:
(e==e' || elem e' ws) && elem e' zs
= def ==, Hipotesis
(True || elem e' ws) && elem e zs
= def ||.1
True && elem e zs
= def &&.2
elem e zs  -- VALE EL CASO
Dem C2:
Lado izq:
if elem e zs then e==e' || elem e' ws && elem e' zs else elem e' ws && elem e' zs
= def ==
if elem e zs then False || elem e' ws && elem e' zs else elem e' ws && elem e' zs
= def ||.2
if elem e zs then elem e' ws && elem e' zs else elem e' ws && elem e' zs
= Lema simplificacion-if
elem e' ws && elem e' zs
Lado der:
(e==e' || elem e' ws) && elem e' zs
= def ==
(False || elem e' ws) && elem e' zs
= def ||.2
elem e' ws && elem e' zs  -- VALE EL CASO VALE LA PROPIEDAD

Lema if-else-and: ¿para todo b1. para todo b2. if b1 then b1 else b2 && b1 = b1?
Dem: sean b1' b2':
C1 b1' = True) ¿if True then True else b2' && True = True?
C2 b1' = False) ¿if False then False else b2' && False = False?
Dem C1:
Lado izq:
if True then True else b2' && True
= eval-if
True  -- VALE EL CASO
Lado der:
if False then False else b2' && False
= eval-if
b2' && False
= conmutatividad-and
False && b2'
= def &&.1
False  -- VALE EL CASO VALE LA PROPIEDAD

Lema elem-diffL: ¿para todo x. para todo xs. para todo ys. elem x (diffL xs ys) = elem x xs && not (elem x ys)?
Dem: sean e zs ws. Por ppio de ind. en la estructura de zs:
CB zs = []) ¿elem e (diffL [] ws) = elem e [] && not (elem e ws)?
CI zs = x':xs') HI) ¡elem e (diffL xs' ws) = elem e xs' && not (elem e ws)!
                TI) ¿elem e (diffL (x':xs') ws) = elem e (x':xs') && not (elem e ws)?
Dem CB:
Lado izq:
elem e (diffL [] ws)
= def diffL.1
elem e []
= def elem.1
False
Lado der:
elem e [] && not (elem e ws)
= def elem.1
False && not (elem e ws)
= def &&.1
False  -- VALE EL CASO
Dem CI:
Lado izq:
elem e (diffL (x':xs') ws)
= def diffL.2
elem e (if elem x' ws then diffL xs' ws else x' : diffL xs' ws)
= prop if-then-else
if elem x' ws then elem e (diffL xs' ws) else elem e (x' : diffL xs' ws)
= def elem.2
if elem x' ws then elem e (diffL xs' ws) else x'==e || elem e (diffL xs' ws)
= HI, HI
if elem x' ws then elem e xs' && not (elem e ws) else x'==e || elem e xs' && not (elem e ws)
= Lema elem-then-not
(x'==e || elem e xs') && not (elem e ws)
Lado der:
elem e (x':xs') && not (elem e ws)
= def elem.2
(x'==e || elem e xs') && not (elem e ws)  -- VALE EL CASO VALE LA PROPIEDAD

Lema elem-then-not: ¿para todo x. para todo y. para todo xs. para todo ys.
                    if elem x xs then elem y ys && not (elem y xs) else x==y || elem y ys && not (elem y xs) = (x==y || elem y ys) && not (elem y xs)?
Dem: sean e e' zs ws:
C1 e==e') ¿if elem e zs then elem e' ws && not (elem e' zs) else e==e' || elem e' ws && not (elem e' zs) = (e==e' || elem e' ws) && not (elem e' zs)?
C2 e/=e') ¿if elem e zs then elem e' ws && not (elem e' zs) else e==e' || elem e' ws && not (elem e' zs) = (e==e' || elem e' ws) && not (elem e' zs)?
Dem C1:
Lado izq:
if elem e zs then elem e' ws && not (elem e' zs) else e==e' || elem e' ws && not (elem e' zs)
= def ==, Hipotesis
if elem e zs then elem e ws && not (elem e zs) else True || elem e ws && not (elem e zs)
= def ||.1
if elem e zs then elem e ws && not (elem e zs) else True && not (elem e zs)
= def &&.2
if elem e zs then elem e ws && not (elem e zs) else not (elem e zs)
= Lema if-then-not
not (elem e zs)
Lado der:
(e==e' || elem e' ws) && not (elem e' zs)
= def ==
(True || elem e' ws) && not (elem e' zs)
= def ||.2
not (elem e' zs)  -- VALE EL CASO
Dem C2:
Lado izq:
if elem e zs then elem e' ws && not (elem e' zs) else e==e' || elem e' ws && not (elem e' zs)
= def ==
if elem e zs then elem e' ws && not (elem e' zs) else False || elem e' ws && not (elem e' zs)
= def ||.2
if elem e zs then elem e' ws && not (elem e' zs) else elem e' ws && not (elem e' zs)
= Lema simplificacion-if
elem e' ws && not (elem e' zs)
Lado der:
(e==e' || elem e' ws) && not (elem e' zs)
= def ==
(False || elem e' ws) && not (elem e' zs)
= def ||.2
elem e' ws && not (elem e' zs)  -- VALE EL CASO VALE LA PROPIEDAD

Lema if-then-not: ¿para todo b1. para todo b2. if b1 then b2 && not b1 else not b1 = not b1?
Dem: sean b1' b2':
C1 b1' = True) ¿if True then b2' && not True else not True = not True?
C2 b1' = False) ¿if False then b2' && not False else not False = not False?
Dem C1:
Lado izq:
if True then b2' && not True else not True
= eval-if
b2' && not True
= def not.1
b2' && False
= Lema conmutatividad-and
False && b2'
False
Lado der:
not True
= def not.1
False  -- VALE EL CASO
Dem C2:
Lado izq:
if False then b2' && not False else not False
= eval-if
not False  -- VALE EL CASO VALE LA PROPIEDAD

DEMO 2
Prop: ¿para todo s. evalSF s = evalSF (simpSE s)?
Dem: sea s'. Por ppio de ind. en la estructura de s':
CB.1 s' = EmptyS) ¿evalSF EmptyS = evalSF (simpSE EmptyS)?
CB.2 s' = Singleton x) ¿evalSF (Singleton x) = evalSF (simpSE (Singleton x))?
CI.1 s' = Union s1 s2) HI11) ¡evalSF s1 = evalSF (simpSE s1)!
                       HI21) ¡evalSF s2 = evalSF (simpSE s2)!
                       TI1) ¿evalSF (Union s1 s2) = evalSF (simpSE (Union s1 s2))?
CI.2 s' = Intersect s1 s2) HI12) ¡evalSF s1 = evalSF (simpSE s1)!
                           HI22) ¡evalSF s2 = evalSF (simpSE s2)!
                           TI2) ¿evalSF (Intersect s1 s2) = evalSF (simpSE (Intersect s1 s2))?
CI.3 s' = Diff s1 s2) HI13) ¡evalSF s1 = evalSF (simpSE s1)!
                      HI23) ¡evalSF s1 = evalSF (simpSE s2)!
                      TI3) ¿evalSF (Diff s1 s2) = evalSF (simpSE (Diff s1 s2))?
Dem CB.1:
Lado der:
evalSF (simpSE EmptyS)
= def simpSE.1
evalSF EmptyS  -- VALE EL CASO
Dem CB.2:
Lado der:
evalSF (simpSE (Singleton x))
= def simpSE.2
evalSF (Singleton x)  -- VALE EL CASO
Dem CI.1:
Lado der:
evalSF (simpSE (Union s1 s2))
= def simpSE.3
evalSF (cfUnion (simpSE s1) (simpSE s2))
= Lema evalSF-cfUnion
\x -> evalSF (simpSE s1) x || evalSF (simpSE s2) x
= HI11, HI12
\x -> evalSF s1 x || evalSF s2 x
Lado izq:
evalSF (Union s1 s2)
= def evalSF.3
\x -> evalSF s1 x || evalSF s2 x  -- VALE EL CASO
Dem CI.2:
Lado der:
evalSF (simpSE (Intersect s1 s2))
= def simpSE.4
evalSF (cfIntersect (simpSE s1) (simpSE s2))
= Lema evalSF-cfIntersect
\x -> evalSF (simpSE s1) x && evalSF (simpSE s2) x
= HI12, HI22
\x -> evalSF s1 x && evalSF s2 x
Lado izq:
evalSF (Intersect s1 s2)
= def evalSF.4
\x -> evalSF s1 x && evalSF s2 x  -- VALE EL CASO
Dem CI.3:
Lado der:
evalSF (simpSE (Diff s1 s2))
= def simpSE.5
evalSF (cfDiff (simpSE s1) (simpSE s2))
= Lema evalSF-cfDiff
\x -> evalSF (simpSE s1) x && not (evalSF (simpSE s2) x)
= HI13, HI23
\x -> evalSF s1 x && not (evalSF s2 x)
Lado izq:
evalSF (Diff s1 s2)
= def evalSF.5
\x -> evalSF s1 x && not (evalSF s2 x)  -- VALE EL CASO VALE LA PROPIEDAD

Lema evalSF-cfUnion: ¿para todo s1. para todo s2. evalSF (cfUnion s1 s2) = \x -> evalSF s1 x || evalSF s2 x?
Dem: por ppio de ext. y por beta
¿para todo s1. para todo s2. para todo x. evalSF (cfUnion s1 s2) x = evalSF s1 x || evalSF s2 x?
sean s1' s2' e:
C1 s1' = EmptyS) ¿evalSF (cfUnion EmptyS s2') e = evalSF EmptyS e || evalSF s2' e?
C2 s1' /= EmptyS y s2' = EmptyS) ¿evalSF (cfUnion s1' EmptyS) e = evalSF s1' e || evalSF EmptyS e?
C3 s1' /= EmptyS y s2' /= EmptyS) ¿evalSF (cfUnion s1' s2') e = evalSF s1' e || evalSF s2' e?
Dem C1:
Lado izq:
evalSF (cfUnion EmptyS s2') e
= def cfUnion.1
evalSF s2' e
Lado der:
evalSF EmptyS e || evalSF s2' e
= def evalSF.1
const False e || evalSF s2' e
= def const
False || evalSF s2' e
= def ||.2
evalSF s2' e  -- VALE EL CASO
Dem C2:
Lado izq:
evalSF (cfUnion s1' EmptyS) e
= def cfUnion.2
evalSF s1' e
Lado der:
evalSF s1' e || evalSF EmptyS e
= def evalSF.1
evalSF s1' e || const False e
= def const
evalSF s1' e || False
= Lema conmutatividad-or
False || evalSF s1' e
= def ||.2
evalSF s1' e  -- VALE EL CASO
Dem C3:
Lado izq:
evalSF (cfUnion s1' s2') e
= def cfUnion.3
evalSF (Union s1' s2') e
= def evalSF.3
(\x -> evalSF s1' x || evalSF s2' x) e
= beta
evalSF s1' e || evalSF s2' e  -- VALE EL CASO VALE LA PROPIEDAD

Lema evalSF-cfIntersect: ¿para todo s1. para todo s2. evalSF (cfIntersect s1 s2) = \x -> evalSF s1 x && evalSF s2 x?
Dem: por ppio de ext. y por beta
¿para todo s1. para todo s2. para todo x. evalSF (cfIntersect s1 s2) x = evalSF s1 x && evalSF s2 x?
sean s1' s2' e:
C1 s1' = EmptyS) ¿evalSF (cfIntersect EmptyS s2') e = evalSF EmptyS e && evalSF s2' e?
C2 s1' /= EmptyS y s2' = EmptyS) ¿evalSF (cfIntersect s1' EmptyS) e = evalSF s1' e && evalSF EmptyS e?
C3 s1' /= EmptyS y s2' /= EmptyS) ¿evalSF (cfIntersect s1' s2') e = evalSF s1' e && evalSF s2' e?
Dem C1:
Lado izq:
evalSF (cfIntersect EmptyS s2') e
= def cfIntersect.1
evalSF EmptyS e
= def evalSF.1
const False e
= def const
False
Lado der:
evalSF EmptyS e && evalSF s2' e
= def evalSF.1
const False e && evalSF s2' e
= def const
False && evalSF s2' e
= def &&.1
False  -- VALE EL CASO
Dem C2:
Lado izq:
evalSF (cfIntersect s1' EmptyS) e
= def cfIntersect.2
evalSF EmptyS e
= def evalSF.1
const False e
= def const
False
Lado der:
evalSF s1' e && evalSF EmptyS e
= def evalSF.1
evalSF s1' e && const False e
= def const
evalSF s1' e && False
= Lema conmutatividad-and
False && evalSF s1' e
= def &&.1
False  -- VALE EL CASO
Dem C3:
Lado izq:
evalSF (cfIntersect s1' s2') e
= def cfIntersect.3
evalSF (Intersect s1' s2') e
= def evalSF.4
(\x -> evalSF s1' x && evalSF s2' x) e
= beta
evalSF s1' e && evalSF s2' e  -- VALE EL CASO VALE LA PROPIEDAD

Lema evalSF-cfDiff: ¿para todo s1. para todo s2. evalSF (cfDiff s1 s2) = \x -> evalSF s1 x && not (evalSF s2 x)?
Dem: por ppio de ext. y por beta
¿para todo s1. para todo s2. para todo x. evalSF (cfDiff s1 s2) x = evalSF s1 x && not (evalSF s2 x)?
sean s1' s2' e:
C1 s1' = EmptyS) ¿evalSF (cfDiff EmptyS s2') e = evalSF EmptyS e && not (evalSF s2' e)?
C2 s1' /= EmptyS y s2' = EmptyS) ¿evalSF (cfDiff s1' EmptyS) e = evalSF s1' e && not (evalSF EmptyS e)?
C3 s1' /= EmptyS y s2' /= EmptyS) ¿evalSF (cfDiff s1' s2') e = evalSF s1' e && not (evalSF s2' e)?
Dem C1:
Lado izq:
evalSF (cfDiff EmptyS s2') e
= def cfDiff.1
evalSF EmptyS e
= def evalSF.1
const False e
= def const
False
Lado der:
evalSF EmptyS e && not (evalSF s2' e)
= def evalSF.1
const False e && not (evalSF s2' e)
= def const
False && not (evalSF s2' e)
= def &&.1
False  -- VALE EL CASO
Dem C2:
Lado izq:
evalSF (cfDiff s1' EmptyS) e
= def cfDiff.2
evalSF s1' e
Lado der:
evalSF s1' e && not (evalSF EmptyS e)
= def evalSF.1
evalSF s1' e && not (const False e)
= def const
evalSF s1' e && not False
= def not.2
evalSF s1' e && True
= Lema conmutatividad-and
True && evalSF s1' e
= def &&.2
evalSF s1' e  -- VALE EL CASO
Dem C3:
Lado izq:
evalSF (cfDiff s1' s2') e
= def cfDiff.3
evalSF (Diff s1' s2') e
= def evalSF.5
(\x -> evalSF s1' x && not (evalSF s2' x)) e
= beta
evalSF s1' e && not (evalSF s2' e)  -- VALE EL CASO VALE LA PROPIEDAD

DEMO 3
Prop: ¿para todo s. evalSL s = evalSL (simpSE s)?
Dem: sea s'. Por ppio de ind. en la estructura de s':
CB.1 s' = EmptyS) ¿evalSL EmptyS = evalSL (simpSE EmptyS)?
CB.2 s' = Singleton x) ¿evalSL (Singleton x) = evalSL (simpSE (Singleton x))?
CI.1 s' = Union s1 s2) HI11) ¡evalSL s1 = evalSL (simpSE s1)!
                       HI21) ¡evalSL s2 = evalSL (simpSE s2)!
                       TI1) ¿evalSL (Union s1 s2) = evalSL (simpSE (Union s1 s2))?
CI.2 s' = Intersect s1 s2) HI12) ¡evalSL s1 = evalSL (simpSE s1)!
                           HI22) ¡evalSL s2 = evalSL (simpSE s2)!
                           TI2) ¿evalSL (Intersect s1 s2) = evalSL (simpSE (Intersect s1 s2))?
CI.3 s' = Diff s1 s2) HI13) ¡evalSL s1 = evalSL (simpSE s1)!
                      HI23) ¡evalSL s2 = evalSL (simpSE s2)!
                      TI3) ¿evalSL (Diff s1 s2) = evalSL (simpSE (Diff s1 s2))?
Dem CB.1:
Lado der:
evalSL (simpSE EmptyS)
= def simpSE.1
evalSL EmptyS  -- VALE EL CASO
Dem CB.2:
Lado der:
evalSL (simpSE (Singleton x))
= def simpSE.2
evalSL (Singleton x)  -- VALE EL CASO
Dem CI.1:
Lado der:
evalSL (simpSE (Union s1 s2))
= def simpSE.3
evalSL (cfUnion (simpSE s1) (simpSE s2))
= Lema evalSL-cfUnion
sinRepetidos (evalSL (simpSE s1) ++ evalSL (simpSE s2))
= HI11, HI21
sinRepetidos (evalSL s1 ++ evalSL s2)
Lado izq:
evalSL (Union s1 s2)
= def evalSL.3
sinRepetidos (evalSL s1 ++ evalSL s2)  -- VALE EL CASO
Dem CI.2:
Lado der:
evalSL (simpSE (Intersect s1 s2))
= def simpSE.4
evalSL (cfIntersect (simpSE s1) (simpSE s2))
= Lema evalSL-cfIntersect
intersectL (evalSL (simpSE s1)) (evalSL (simpSE s2))
= HI12, HI22
intersectL (evalSL s1) (evalSL s2)
Lado izq:
evalSL (Intersect s1 s2)
= def evalSL.4
intersectL (evalSL s1) (evalSL s2)  -- VALE EL CASO
Dem CI.3:
Lado der:
evalSL (simpSE (Diff s1 s2))
= def simpSE.5
evalSL (cfDiff (simpSE s1) (simpSE s2))
= Lema evalSL-cfDiff
diffL (evalSL (simpSE s1)) (evalSL (simpSE s2))
= HI13, HI23
diffL (evalSL s1) (evalSL s2)
Lado izq:
evalSL (Diff s1 s2)
= def evalSL.5
diffL (evalSL s1) (evalSL s2)  -- VALE EL CASO VALE LA PROPIEDAD

Lema evalSL-cfUnion: ¿para todo s1. para todo s2. si (para todo s. sinRepetidos (evalSL s) = evalSL s)
                      entonces evalSL (cfUnion s1 s2) = sinRepetidos (evalSL s1 ++ evalSL s2)?
Dem: sean s1' s2':
C1 s1' = EmptyS) ¿si (para todo s. sinRepetidos (evalSL s) = evalSL s)
                  entonces evalSL (cfUnion EmptyS s2') = sinRepetidos (evalSL EmptyS ++ evalSL s2')?
C2 s1' /= EmptyS y s2' = EmptyS) ¿si (para todo s. sinRepetidos (evalSL s) = evalSL s)
                                  entonces evalSL (cfUnion s1' EmptyS) = sinRepetidos (evalSL s1' ++ evalSL EmptyS)?
C3 s1' /= EmptyS y s2' /= EmptyS) ¿si (para todo s. sinRepetidos (evalSL s) = evalSL s)
                                   entonces evalSL (cfUnion s1' s2') = sinRepetidos (evalSL s1' ++ evalSL s2')?
Dem C1:
Lado izq:
evalSL (cfUnion EmptyS s2')
= def cfUnion.1
evalSL s2'
Lado der:
sinRepetidos (evalSL EmptyS ++ evalSL s2')
= def evalSL.1
sinRepetidos ([] ++ evalSL s2')
= def ++.1
sinRepetidos (evalSL s2')
= Hipotesis de la propiedad, con s = s2'
evalSL s2'  -- VALE EL CASO
Dem C2:
Lado izq:
evalSL (cfUnion s1' EmptyS)
= def cfUnion.2
evalSL s1'
Lado der:
sinRepetidos (evalSL s1' ++ evalSL EmptyS)
= def evalSL.1
sinRepetidos (evalSL s1' ++ [])
= Lema append-nil
sinRepetidos (evalSL s1')
= Hipotesis de la propiedad, con s = s1'
evalSL s1'  -- VALE EL CASO
Dem C3:
Lado izq:
evalSL (cfUnion s1' s2')
= def cfUnion.3
evalSF (Union s1' s2')
= def evalSF.3
sinRepetidos (evalSF s1' ++ evalSF s2')  -- VALE EL CASO VALE LA PROPIEDAD

Lema append-nil: ¿para todo xs. xs ++ [] = xs?
Dem: sea zs por ppio de ind. en la estructura de zs:
CB zs = []) ¿[] ++ [] = []?
CI zs = x:xs') HI) ¡xs' ++ [] = xs'!
               TI) ¿(x:xs') ++ [] = x:xs'?
Dem CB:
Lado izq:
[] ++ []
= def ++.1
[]  -- VALE EL CASO
Dem CI:
Lado izq:
(x:xs') ++ []
= def ++.2
x : xs' ++ []
= HI
x : xs'  -- VALE EL CASO VALE LA PROPIEDAD

Lema evalSL-cfIntersect: ¿para todo s1. para todo s2. evalSL (cfIntersect s1 s2) = intersectL (evalSL s1) (evalSL s2)?
Dem: sean s1' s2':
C1 s1' = EmptyS) ¿evalSL (cfIntersect EmptyS s2') = intersectL (evalSL EmptyS) (evalSL s2')?
C2 s1' /= EmptyS y s2' = EmptyS) ¿evalSL (cfIntersect s1' EmptyS) = intersectL (evalSL s1') (evalSL EmptyS)?
C3 s1' /= EmptyS y s2' /= EmptyS) ¿evalSL (cfIntersect s1' s2') = intersectL (evalSL s1') (evalSL s2')?
Dem C1:
Lado izq:
evalSL (cfIntersect EmptyS s2')
= def cfIntersect.1
evalSL EmptyS
= def evalSL.1
[]
Lado der:
intersectL (evalSL EmptyS) (evalSL s2')
= def evalSL.1
intersectL [] (evalSL s2')
= def intersectL.1
[]  -- VALE EL CASO
Dem C2:
Lado izq:
evalSL (cfIntersect s1' EmptyS)
= def cfIntersect.2
evalSL EmptyS
= def evalSL.1
[]
Lado der:
intersectL (evalSL s1') (evalSL EmptyS)
= def evalSL.1
intersectL (evalSL s1') []
= Lema intersectL-nil
[]  -- VALE EL CASO
Dem C3:
Lado izq:
evalSL (cfIntersect s1' s2')
= def cfIntersect.3
evalSL (Intersect s1' s2')
= def evalSL.4
intersectL (evalSL s1') (evalSL s2')  -- VALE EL CASO VALE LA PROPIEDAD

Lema intersectL-nil: ¿para todo xs. intersectL xs [] = []?
Dem: sea zs. por ppio de ind. en la estructura de zs:
CB zs = []) ¿intersectL [] [] = []?
CI zs = x:xs') HI) ¡intersectL xs' [] = []!
               TI) ¿intersectL (x:xs') [] = []?
Dem CB:
Lado izq:
intersectL [] []
= def intersectL.1
[]  -- VALE EL CASO
Dem CI:
Lado izq:
intersectL (x:xs') []
= def intersectL.2
if elem x [] then x : intersectL xs' [] else intersectL xs' []
= def elem.1, HI, HI
if False then x : [] else []
= eval-if
[]  -- VALE EL CASO VALE LA PROPIEDAD

Lema evalSL-cfDiff: ¿para todo s1. para todo s2. evalSL (cfDiff s1 s2) = diffL (evalSL s1) (evalSL s2)?
Dem: sean s1' s2':
C1 s1' = EmptyS) ¿evalSL (cfDiff EmptyS s2') = diffL (evalSL EmptyS) (evalSL s2')?
C2 s1' /= EmptyS y s2' = EmptyS) ¿evalSL (cfDiff s1' EmptyS) = diffL (evalSL s1') (evalSL EmptyS)?
C3 s1' /= EmptyS y s2' /= EmptyS) ¿evalSL (cfDiff s1' s2') = diffL (evalSL s1') (evalSL s2')?
Dem C1:
Lado izq:
evalSL (cfDiff EmptyS s2')
= def cfDiff.1
evalSL EmptyS
= def evalSL.1
[]
Lado der:
diffL (evalSL EmptyS) (evalSL s2')
= def evalSL.1
diffL [] (evalSL s2')
= def diffL.1
[]  -- VALE EL CASO
Dem C2:
Lado izq:
evalSL (cfDiff s1' EmptyS)
= def cfDiff.2
evalSL s1'
Lado der:
diffL (evalSL s1') (evalSL EmptyS)
= def evalSL.1
diffL (evalSL s1') []
= Lema diffL-nil
evalSL s1'  -- VALE EL CASO
Dem C3:
Lado izq:
evalSL (cfDiff s1' s2')
= def cfDiff.3
evalSL (Diff s1' s2')
= def evalSL.5
diffL (evalSL s1') (evalSL s2')  -- VALE EL CASO VALE LA PROPIEDAD

Lema diffL-nil: ¿para todo xs. diffL xs [] = xs?
Dem: sea zs. Por ppio de ind. en la estructura de zs:
CB zs = []) ¿diffL [] [] = []?
CI zs = x:xs') HI) ¡diffL xs' [] = xs'!
               TI) ¿diffL (x:xs') [] = x:xs'?
Dem CB:
Lado izq:
diffL [] []
= def diffL.1
[]  -- VALE EL CASO
Dem CI:
Lado izq:
diffL (x:xs') []
= def diffL.2
if elem x [] then diffL xs' [] else x : diffL xs' []
= def elem.1, HI, HI
if False then xs' else x : xs'
= eval-if
x : xs'  -- VALE EL CASO VALE LA PROPIEDAD

DEMO 4
Prop: ¿para todo xs. para todo s. evalSF (toSetExp xs (evalSF s)) = evalSF s?
Contraejemplo:
s = Union (Singleton 1) (Singleton 2)
xs = [1]