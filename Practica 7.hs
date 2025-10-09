-- Seccion 1
data Pizza = Prepizza | Capa Ingrediente Pizza
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa

-- Las reglas que definen al conjunto inductivo Pizza son las siguientes:
-- caso base) Prepizza pertenece al conjunto.
-- caso inductivo) Si i pertenece al conjunto Ingrediente y p al conjunto Pizza entonces Capa i p pertenece al conjunto Pizza.

-- Las reglas que definen al conjunto Ingrediente son:
-- caso 1) si n pertenece a Int, entonces Aceitunas n pertenece a Ingrediente.
-- caso 2) Anchoas pertenece al conjunto Ingrediente.
-- caso 3) Cebolla pertenece al conjunto Ingrediente.
-- caso 4) Jamon pertenece al conjunto Ingrediente.
-- caso 5) Queso pertenece al conjunto Ingrediente.
-- caso 6) Salsa pertenece al conjunto Ingrediente.

-- Ejercicio 3 A
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ pizza) = 1 + cantidadDeCapas pizza

-- Ejercicio 3 B
cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza   = 0
cantidadDeAceitunas (Capa i p) = aceitunasEnIngrediente i + cantidadDeAceitunas p

aceitunasEnIngrediente :: Ingrediente -> Int
aceitunasEnIngrediente (Aceitunas n) = n
aceitunasEnIngrediente _             = 0

-- Ejercicio 3 C
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = Capa (duplicarEnIngrediente i) (duplicarAceitunas p)

duplicarEnIngrediente :: Ingrediente -> Ingrediente
duplicarEnIngrediente (Aceitunas n) = Aceitunas (2 * n)
duplicarEnIngrediente i             = i

-- Ejercicio 3 D
sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza   = Prepizza
sinLactosa (Capa i p) = removerLactosaDeIngrediente i (sinLactosa p)

removerLactosaDeIngrediente :: Ingrediente -> Pizza -> Pizza
removerLactosaDeIngrediente Queso = id
removerLactosaDeIngrediente i     = Capa i

-- Ejercicio 3 E
aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza   = True
aptaIntolerantesLactosa (Capa i p) = ingredienteSinLactosa i && aptaIntolerantesLactosa p

ingredienteSinLactosa :: Ingrediente -> Bool
ingredienteSinLactosa Queso = False
ingredienteSinLactosa _     = True

-- Ejercicio 3 F
conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza   = Prepizza
conDescripcionMejorada (Capa i p) = juntarAceitunas i (conDescripcionMejorada p)

juntarAceitunas :: Ingrediente -> Pizza -> Pizza
juntarAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n + m)) p
juntarAceitunas i p                                  = Capa i p

-- Alternativa sin doble PM ni PM anidados
-- juntarAceitunas :: Ingrediente -> Pizza -> Pizza
-- juntarAceitunas (Aceitunas n) p = juntarAceitunasSi (tieneInmediatamenteAceitunas p) n p
-- juntarAceitunas i             p = Capa i p
--
-- tieneInmediatamenteAceitunas :: Pizza -> Bool
-- tieneInmediatamenteAceitunas (Capa i _) = esAceitunas i
-- tieneInmediatamenteAceitunas _          = False
--
-- esAceitunas :: Ingrediente -> Bool
-- esAceitunas (Aceitunas _) = True
-- esAceitunas _             = False
--
-- juntarAceitunasSi :: Bool -> Int -> Pizza -> Pizza
-- juntarAceitunasSi True n p = agregarAceitunas n p
-- juntarAceitunasSi _    n p = Capa (Aceitunas n) p
--
-- agregarAceitunas :: Int -> Pizza -> Pizza
-- agregarAceitunas n Prepiza    = Prepizza
-- agregarAceitunas n (Capa i p) = Capa (agregarAceitunasIngrediente) p
--
-- agregarAceitunasIngrediente :: Ingrediente -> Ingrediente
-- agregarAceitunasIngrediente (Aceitunas m) = Aceitunas (n + m)
-- agregarAceitunasIngrediente i             = i

-- Seccion 2
type Nombre = String
data Planilla = Fin | Registro Nombre Planilla
data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo

-- Ejercicio 3 A
largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin            = 0
largoDePlanilla (Registro _ p) = 1 + largoDePlanilla p

-- Ejercicio 3 B
esta :: Nombre -> Planilla -> Bool
esta _ Fin             = False
esta n (Registro n' p) = n == n' || esta n p

-- Ejercicio 3 C
juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas Fin             p = p
juntarPlanillas (Registro n p') p = Registro n (juntarPlanillas p' p)

-- Ejercicio 3 D
nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario _)               = 0
nivelesJerarquicos (Investigador _ e1 e2 e3) = 1 + maximum [nivelesJerarquicos e1, nivelesJerarquicos e2, nivelesJerarquicos e3]

-- Ejercicio 3 E
cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario _)               = 1
cantidadDeIntegrantes (Investigador _ e1 e2 e3) = 1 + cantidadDeIntegrantes e1 + cantidadDeIntegrantes e2 + cantidadDeIntegrantes e3

-- Ejercicio 3 F
planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario n)               = Registro n Fin
planillaDeIntegrantes (Investigador n e1 e2 e3) = Registro n (planillaDeIntegrantes e1 `juntarPlanillas` planillaDeIntegrantes e2 `juntarPlanillas` planillaDeIntegrantes e3)

-- Seccion 3
data Dungeon a = Habitacion a | Pasaje (Maybe a) (Dungeon a) | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a)

-- Sea A cualquier conjunto, voy a definir el conjunto inductivo Dungeon A con las siguientes reglas:
-- caso base) si x pertenece al conjunto A entonces Habitacion x pertenece a Dungeon A.
-- caso inductivo 1) si m pertenece a Maybe A y d pertenece a Dungeon A entonces Pasaje m d pertenece a Dungeon A.
-- caso inductivo 2) si m pertenece a Maybe A y d1 y d2 pertenecen a Dungeon A entonces Bifurcacion m d1 d2 pertenece a Dungeon A.

-- Ejercicio 3 A
cantidadDeBifurcaciones :: Dungeon a -> Int
cantidadDeBifurcaciones (Habitacion _)        = 0
cantidadDeBifurcaciones (Pasaje _ d)          = cantidadDeBifurcaciones d
cantidadDeBifurcaciones (Bifurcacion _ d1 d2) = 1 + cantidadDeBifurcaciones d1 + cantidadDeBifurcaciones d2

-- Ejercicio 3 B
cantidadDePuntosInteresantes :: Dungeon a -> Int
cantidadDePuntosInteresantes (Habitacion _)        = 0
cantidadDePuntosInteresantes (Pasaje m d)          = 1 + cantidadDePuntosInteresantes d
cantidadDePuntosInteresantes (Bifurcacion m d1 d2) = 1 + cantidadDePuntosInteresantes d1 + cantidadDePuntosInteresantes d2
-- maybe 0 (\_ -> 1) m: es equivalente a case ma of Nothing -> 0; Just _ -> 1

-- Ejercicio 3 C
cantidadDePuntosVacios :: Dungeon a -> Int
cantidadDePuntosVacios (Habitacion _)        = 0
cantidadDePuntosVacios (Pasaje m d)          = maybe 1 (const 0) m + cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion m d1 d2) = maybe 1 (const 0) m + cantidadDePuntosVacios d1 + cantidadDePuntosVacios d2

-- Ejercicio 3 D
cantidadDePuntosCon :: Eq a => a -> Dungeon a -> Int
cantidadDePuntosCon x (Habitacion x')       = unoSi (x == x')
cantidadDePuntosCon x (Pasaje m d)          = unoSi (isJust m && fromJust m == x) + cantidadDePuntosCon x d
cantidadDePuntosCon x (Bifurcacion m d1 d2) = unoSi (isJust m && fromJust m == x) + cantidadDePuntosCon x d1 + cantidadDePuntosCon x d2

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

fromJust :: Maybe a -> a
-- PRECOND: No se aplica a Nothing
fromJust (Just x) = x
fromJust _        = error "No se puede extraer valor de Nothing"

unoSi :: Bool -> Int
unoSi True = 1
unoSi _    = 0

-- Ejercicio 3 E
esLineal :: Dungeon a -> Bool
esLineal (Habitacion _)      = True
esLineal (Pasaje _ d)        = esLineal d
esLineal (Bifurcacion _ _ _) = False

-- Ejercicio 3 F
llenoDe :: Eq a => a -> Dungeon a -> Bool
llenoDe x (Habitacion x')       = x == x'
llenoDe x (Pasaje m d)          = maybe False (== x) m && llenoDe x d
llenoDe x (Bifurcacion m d1 d2) = maybe False (== x) m && llenoDe x d1 && llenoDe x d2

-- Ejercicios Adicionales
-- Ejercicio 1
-- Completar las reglas del conjunto inductivo, definir el tipo algebraico basado en esas reglas y modificar el esquema para contemplar las mismas reglas.
-- Las reglas que definen el conjunto inductivo FilaColores
-- 1) Caso base: CeldaFinal está en FilaColores.
-- 2) Caso Inductivo: si c es un Color y fila está en FilaColores entonces Celda c fila está en FilaColores.

data Color = Rojo | Verde | Negro | Azul
     deriving Show

data FilaColores = Celda Color FilaColores | CeldaFinal
     deriving Show

-- f :: FilaColores -> ...
-- f CeldaFinal     = ...
-- f (Celda c fila) = ... f fila ...

-- Ejercicio 2
-- Modificar las funciones definidas con la forma anterior de tipo algebraico FilaColores para que utilicen el nuevo esquema recursivo estructural.

longitud :: FilaColores -> Int
longitud CeldaFinal     = 0
longitud (Celda _ fila) = 1 + longitud fila

soloRojo :: FilaColores -> FilaColores
soloRojo CeldaFinal     = CeldaFinal
soloRojo (Celda c fila) = construirSiEsRojo c (soloRojo fila)

construirSiEsRojo :: Color -> FilaColores -> FilaColores
construirSiEsRojo Rojo = Celda Rojo
construirSiEsRojo _    = id

contarCeldasDeColor :: Color -> FilaColores -> Int
contarCeldasDeColor _ CeldaFinal      = 0
contarCeldasDeColor c (Celda c' fila) = unoSiIguales c c' + contarCeldasDeColor c fila

unoSiIguales :: Color -> Color -> Int
unoSiIguales Rojo  Rojo  = 1
unoSiIguales Verde Verde = 1
unoSiIguales Azul  Azul  = 1
unoSiIguales Negro Negro = 1
unoSiIguales _     _     = 0

-- Ejercicio 3
-- Dar las reglas del conjunto inductivo que defina cadenas con simbolos A y B, pero que si o si vengan de a pares AA y BB.
-- Las reglas que definen el conjunto inductivo CadenasAABB
-- 1) Caso base: si x esta en AABB entonces Base x esta en CadenasAABB.
-- 3) Caso Inductivo: si x esta en AABB y cadena esta en CadenasAABB entonces Cadena x cadena esta en CadenasAABB.

data AABB = AA | BB

data CadenasAABB = Base AABB | Cadena AABB CadenasAABB

-- Tarea: Modificar el codigo
-- Alternativa 1: basada en agregar un color a una fila
coloresAIzq :: Dungeon Color -> FilaColores
coloresAIzq (Habitacion x)      = addColor (Just x) CeldaFinal
coloresAIzq (Pasaje m d)        = addColor m (coloresAIzq d)
coloresAIzq (Bifurcacion m d _) = addColor m (coloresAIzq d)

addColor :: Maybe Color -> FilaColores -> FilaColores
addColor (Just c) = Celda c
addColor _        = id

-- Alternativa 2: basada en unir filas
coloresAIzq' :: Dungeon Color -> FilaColores
coloresAIzq' (Habitacion x)      = mColorAFila (Just x)
coloresAIzq' (Pasaje m d)        = mColorAFila m `concatFilas` (coloresAIzq' d)
coloresAIzq' (Bifurcacion m d _) = mColorAFila m `concatFilas` (coloresAIzq' d)

mColorAFila :: Maybe Color -> FilaColores
mColorAFila (Just c) = Celda c CeldaFinal
mColorAFila _        = CeldaFinal

concatFilas :: FilaColores -> FilaColores -> FilaColores
concatFilas CeldaFinal      fila = fila
concatFilas (Celda c fila') fila = Celda c (concatFilas fila' fila)

-- Ejemplos
-- Fila vacia
filaVacia = CeldaFinal

-- Fila con un solo color
filaUnColor = Celda Rojo CeldaFinal

-- Fila con varios colores diferentes
filaColores = Celda Rojo (Celda Verde (Celda Azul (Celda Negro CeldaFinal)))

-- Fila con colores repetidos
filaRepetidos = Celda Rojo (Celda Rojo (Celda Azul (Celda Azul CeldaFinal)))

-- Fila con un patron alternado
filaAlternada = Celda Rojo (Celda Azul (Celda Rojo (Celda Azul CeldaFinal)))

-- Fila con todos los colores disponibles
filaTodosColores = Celda Rojo (Celda Verde (Celda Azul (Celda Negro CeldaFinal)))

-- Fila larga con un solo color
filaLargaUnColor = Celda Rojo (Celda Rojo (Celda Rojo (Celda Rojo CeldaFinal)))

-- Fila con un solo color al final
filaColorFinal = Celda Verde CeldaFinal

-- Fila con colores que no incluyen Rojo
filaSinRojo = Celda Azul (Celda Verde (Celda Negro CeldaFinal))

-- Fila con solo un color repetido muchas veces
filaSoloAzul = Celda Azul (Celda Azul (Celda Azul CeldaFinal))

-- Dungeon con una sola habitacion
dungeonSimple = Habitacion Rojo

-- Dungeon lineal con un pasaje
dungeonLineal = Pasaje (Just Azul) (Habitacion Verde)

-- Dungeon con bifurcacion simple
dungeonBifurcacion = Bifurcacion (Just Rojo) (Habitacion Azul) (Habitacion Verde)

-- Dungeon con bifurcaciones anidadas
dungeonAnidado = Bifurcacion (Just Negro)
                  (Pasaje (Just Rojo) (Habitacion Azul))
                  (Bifurcacion (Just Verde) (Habitacion Rojo) (Habitacion Azul))

-- Dungeon con pasajes vacios (Nothing)
dungeonConVacios = Pasaje Nothing
                    (Bifurcacion Nothing
                      (Pasaje (Just Azul) (Habitacion Verde))
                      (Habitacion Rojo))

-- Dungeon completamente lleno de un solo color
dungeonLlenoDeRojo = Bifurcacion (Just Rojo)
                       (Pasaje (Just Rojo) (Habitacion Rojo))
                       (Habitacion Rojo)

-- Dungeon con solo Nothing en los pasajes y bifurcaciones
dungeonSoloVacios = Bifurcacion Nothing
                     (Pasaje Nothing (Habitacion Azul))
                     (Pasaje Nothing (Habitacion Verde))

-- Dungeon con una bifurcacion que lleva a dos dungeons lineales
dungeonMixto = Bifurcacion (Just Azul)
                  (Pasaje (Just Rojo) (Pasaje (Just Verde) (Habitacion Negro)))
                  (Pasaje (Just Azul) (Habitacion Rojo))