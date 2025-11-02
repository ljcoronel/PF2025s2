-- Ejercicio 1
data NExp = Var Variable | NCte Int | NBOp NBinOp NExp NExp
data NBinOp = Add | Sub | Mul | Div | Mod | Pow
type Variable = String

-- TAD Memoria
type Memoria = String

enBlanco :: Memoria
enBlanco = undefined

cuantoVale :: Variable -> Memoria -> Maybe Int
cuantoVale x mem = undefined

recordar :: Variable -> Int -> Memoria -> Memoria
recordar x n mem = undefined

variables :: Memoria -> [Variable]
variables mem = undefined

-- Ejercicio i
evalNExp :: NExp -> Memoria -> Int
evalNExp (Var x)            mem = case cuantoVale x mem of
                                    Nothing -> error ("Variable "++x++" indefinida")
                                    Just n  -> n
evalNExp (NCte n)           mem = n
evalNExp (NBOp bop ne1 ne2) mem = evalNBOp bop (evalNExp ne1 mem) (evalNExp ne2 mem)

evalNBOp :: NBinOp -> Int -> Int -> Int
evalNBOp Add = (+)
evalNBOp Sub = (-)
evalNBOp Mul = (*)
evalNBOp Div = div
evalNBOp Mod = mod
evalNBOp Pow = (^)

-- Ejercicio ii
cfNExp :: NExp -> NExp
cfNExp (Var x)            = Var x
cfNExp (NCte n)           = NCte n
cfNExp (NBOp bop ne1 ne2) = cfBOp bop (cfNExp ne1) (cfNExp ne2)

cfBOp :: NBinOp -> NExp -> NExp -> NExp
cfBOp bop (NCte n) (NCte m) = NCte (evalNBOp bop n m)
cfBOp bop ne1       ne2     = NBOp bop ne1 ne2

-- Ejercicios adicionales
cantExtra :: NExp -> Int
-- PROP: Devuelve la cantidad extra de sub-NExp que se podrian optimizar.
cantExtra (Var _)          = 0
cantExtra (NCte _)         = 0
cantExtra (NBOp _ ne1 ne2) = unoSiPuedeOptimizar ne1 ne2 + cantExtra ne1 + cantExtra ne2

unoSiPuedeOptimizar :: NExp -> NExp -> Int
unoSiPuedeOptimizar (NCte _) (NCte _) = 1
unoSiPuedeOptimizar _        _        = 0

cfNE :: NExp -> NExp
-- PROP: Devuelve una NExp sin expresiones optimizables.
cfNE (Var x)            = Var x
cfNE (NCte n)           = NCte n
cfNE (NBOp bop ne1 ne2) = cfBOp bop (cfNE ne1) (cfNE ne2)

-- Ejercicio 2
data BExp = BCte Bool | Not BExp | And BExp BExp | Or BExp BExp | ROp RelOp NExp NExp
data RelOp = Eq | NEq -- Equal y NotEqual
           | Gt | GEq -- Greater y GreaterOrEqual
           | Lt | LEq -- Lower y LowerOrEqual

-- Ejercicio i
evalBExp :: BExp -> Memoria -> Bool
evalBExp (BCte b)          mem = b
evalBExp (Not be)          mem = not (evalBExp be mem)
evalBExp (And be1 be2)     mem = evalBExp be1 mem && evalBExp be2 mem
evalBExp (Or be1 be2)      mem = evalBExp be1 mem || evalBExp be2 mem
evalBExp (ROp rop ne1 ne2) mem = evalROp rop (evalNExp ne1 mem) (evalNExp ne2 mem)

evalROp :: RelOp -> Int -> Int -> Bool
evalROp Eq  = (==)
evalROp NEq = (/=)
evalROp Gt  = (>)
evalROp GEq = (>=)
evalROp Lt  = (<)
evalROp LEq = (<=)

-- Ejercicio ii
cfBExp :: BExp -> BExp
cfBExp (BCte b)          = BCte b
cfBExp (Not be)          = cfNot (cfBExp be)
cfBExp (And be1 be2)     = cfAnd (cfBExp be1) (cfBExp be2)
cfBExp (Or be1 be2)      = cfOr (cfBExp be1) (cfBExp be2)
cfBExp (ROp rop ne1 ne2) = cfROp rop (cfNExp ne1) (cfNExp ne2)

cfNot :: BExp -> BExp
cfNot (BCte b) = BCte (not b)
cfNot be       = Not be

cfAnd :: BExp -> BExp -> BExp
cfAnd (BCte b) be  = if b
                      then be
                      else BCte False
cfAnd be1      be2 = And be1 be2

cfOr :: BExp -> BExp -> BExp
cfOr (BCte b) be  = if b
                     then BCte True
                     else be
cfOr be1      be2 = Or be1 be2

cfROp :: RelOp -> NExp -> NExp -> BExp
cfROp rop (NCte n) (NCte m) = BCte (evalROp rop n m)
cfROp rop ne1      ne2      = ROp rop ne1 ne2

-- Ejercicio 3
data Programa = Prog Bloque
type Bloque = [Comando]
data Comando = Assign Nombre NExp | If BExp Bloque Bloque | While BExp Bloque
type Nombre = String

-- Ejericicio i
evalProg :: Programa -> Memoria -> Memoria
evalProg (Prog cs) mem = evalBlq cs mem

-- Ejericicio ii
evalBlq :: Bloque -> Memoria -> Memoria
evalBlq []     mem = mem
evalBlq (c:cs) mem = evalBlq cs (evalCom c mem)

-- Ejericicio iii
evalCom :: Comando -> Memoria -> Memoria
evalCom (Assign x ne)   mem = recordar x (evalNExp ne mem) mem
evalCom (If be cs1 cs2) mem = if evalBExp be mem
                               then evalBlq cs1 mem
                               else evalBlq cs2 mem
evalCom (While be cs)   mem = evalCom (If be (cs ++ [While be cs]) []) mem

-- Ejericicio iv
optimizeCF :: Programa -> Programa
optimizeCF (Prog cs) = Prog (cfBlq cs)

cfBlq :: Bloque -> Bloque
cfBlq []     = []
cfBlq (c:cs) = cfCom c ++ cfBlq cs

cfCom :: Comando -> Bloque
cfCom (Assign x ne)   = [Assign x (cfNExp ne)]
cfCom (If be cs1 cs2) = cfIf (cfBExp be) (cfBlq cs1) (cfBlq cs2)
cfCom (While be cs)   = cfWhile (cfBExp be) (cfBlq cs)

cfIf :: BExp -> Bloque -> Bloque -> Bloque
cfIf (BCte b) cs1 cs2 = if b
                         then cs1
                         else cs2
cfIf be       cs1 cs2 = [If be cs1 cs2]

cfWhile :: BExp -> Bloque -> Bloque
cfWhile (BCte False) _  = []
cfWhile be           cs = [While be cs]

-- Ejercicio 4
data DirR = Oeste | Este
data ExpR a = Lit a | PuedeMover DirR | NroBolitas | HayBolitas | UnOp UOp (ExpR a) | BinOp BOp (ExpR a) (ExpR a)
data UOp = No | Siguiente | Previo
data BOp = YTambien | OBien | Mas | Por
type ProgramaR = ComandoR
data ComandoR = Mover DirR | Poner | Sacar | NoOp | Repetir (ExpR Int) ComandoR | Mientras (ExpR Bool) ComandoR | Secuencia ComandoR ComandoR

-- TAD TableroR
type TableroR = String

tableroInicial :: Int -> TableroR
tableroInicial n = undefined

mover :: DirR -> TableroR -> TableroR
mover d t = undefined

poner :: TableroR -> TableroR
poner t = undefined

sacar :: TableroR -> TableroR
sacar t = undefined

nroBolitas :: TableroR -> Int
nroBolitas t = undefined

hayBolitas :: TableroR -> Bool
hayBolitas t = undefined

puedeMover :: DirR -> TableroR -> Bool
puedeMover d t = undefined

boom :: String -> TableroR -> a
boom s t = undefined

-- Ejercicio i
evalExpRInt :: ExpR Int -> TableroR -> Int
evalExpRInt (Lit n)           _ = n
evalExpRInt (PuedeMover _)    t = boom "Operacion invalida" t
evalExpRInt NroBolitas        t = nroBolitas t
evalExpRInt HayBolitas        t = boom "Operacion invalida" t
evalExpRInt (UnOp uop e)      t = evalUOpInt uop t (evalExpRInt e t)
evalExpRInt (BinOp bop e1 e2) t = evalBOpInt bop t (evalExpRInt e1 t) (evalExpRInt e2 t)

evalUOpInt :: UOp -> TableroR -> Int -> Int
evalUOpInt Siguiente _ n = n+1
evalUOpInt Previo    _ n = n-1
evalUOpInt _         t _ = boom "Operacion invalida" t

evalBOpInt :: BOp -> TableroR -> Int -> Int -> Int
evalBOpInt Mas _ = (+)
evalBOpInt Por _ = (*)
evalBOpInt _   t = boom "Operacion invalida" t

-- Ejercicio ii
evalExpRBool :: ExpR Bool -> TableroR -> Bool
evalExpRBool (Lit b)           _ = b
evalExpRBool (PuedeMover d)    t = puedeMover d t
evalExpRBool NroBolitas        t = boom "Operacion invalida" t
evalExpRBool HayBolitas        t = hayBolitas t
evalExpRBool (UnOp uop e)      t = evalUOpBool uop t (evalExpRBool e t)
evalExpRBool (BinOp bop e1 e2) t = evalBOpBool bop t (evalExpRBool e1 t) (evalExpRBool e2 t)

evalUOpBool :: UOp -> TableroR -> Bool -> Bool
evalUOpBool No _ = not
evalUOpBool _  t = boom "Operacion invalida" t

evalBOpBool :: BOp -> TableroR -> Bool -> Bool -> Bool
evalBOpBool YTambien _ = (&&)
evalBOpBool OBien    _ = (||)
evalBOpBool _        t = boom "Operacion invalida" t

-- Ejercicio iii
expRTieneTipoInt :: ExpR Int -> Bool
expRTieneTipoInt (Lit _)           = True
expRTieneTipoInt (PuedeMover _)    = False
expRTieneTipoInt NroBolitas        = True
expRTieneTipoInt HayBolitas        = False
expRTieneTipoInt (UnOp uop e)      = uOpTieneTipoInt uop && (expRTieneTipoInt e)
expRTieneTipoInt (BinOp bop e1 e2) = bOpTieneTipoInt bop && (expRTieneTipoInt e1) && (expRTieneTipoInt e2)

uOpTieneTipoInt :: UOp -> Bool
uOpTieneTipoInt No = False
uOpTieneTipoInt _  = True

bOpTieneTipoInt :: BOp -> Bool
bOpTieneTipoInt Mas = True
bOpTieneTipoInt Por = True
bOpTieneTipoInt _   = False

-- Ejercicio iv
expRTieneTipoBool :: ExpR Bool -> Bool
expRTieneTipoBool (Lit _)           = True
expRTieneTipoBool (PuedeMover _)    = True
expRTieneTipoBool NroBolitas        = False
expRTieneTipoBool HayBolitas        = True
expRTieneTipoBool (UnOp uop e)      = uOpTieneTipoBool uop && (expRTieneTipoBool e)
expRTieneTipoBool (BinOp bop e1 e2) = bOpTieneTipoBool bop && (expRTieneTipoBool e1) && (expRTieneTipoBool e2)

uOpTieneTipoBool :: UOp -> Bool
uOpTieneTipoBool No = True
uOpTieneTipoBool _  = False

bOpTieneTipoBool :: BOp -> Bool
bOpTieneTipoBool YTambien = True
bOpTieneTipoBool OBien    = True
bOpTieneTipoBool _        = False

-- Ejercicio v
evalR :: ComandoR -> TableroR -> TableroR
evalR (Mover d)         t = mover d t
evalR Poner             t = poner t
evalR Sacar             t = sacar t
evalR NoOp              t = t
evalR (Repetir e c)     t = case mEvalExpRInt e t of
                             Nothing -> t
                             Just n  -> evalR (Repetir (Lit (n-1)) c) (evalR c t)
evalR (Mientras e c)    t = case mEvalExpRBool e t of
                             Nothing -> t
                             Just b  -> evalR (Mientras (Lit b) c) (evalR c t)
evalR (Secuencia c1 c2) t = evalR c1 (evalR c2 t)

mEvalExpRInt :: ExpR Int -> TableroR -> Maybe Int
mEvalExpRInt (Lit n)           _ = Just n
mEvalExpRInt (PuedeMover _)    _ = Nothing
mEvalExpRInt NroBolitas        t = Just (nroBolitas t)
mEvalExpRInt HayBolitas        t = Nothing
mEvalExpRInt (UnOp uop e)      t = mUOpInt uop (mEvalExpRInt e t)
mEvalExpRInt (BinOp bop e1 e2) t = mBOpInt bop (mEvalExpRInt e1 t) (mEvalExpRInt e2 t)

mUOpInt :: UOp -> Maybe Int -> Maybe Int
mUOpInt Siguiente (Just n) = Just (n+1)
mUOpInt Previo    (Just n) = Just (n-1)
mUOpInt _         _        = Nothing

mBOpInt :: BOp -> Maybe Int -> Maybe Int -> Maybe Int
mBOpInt Mas (Just n) (Just m) = Just (n+m)
mBOpInt Por (Just n) (Just m) = Just (n*m)
mBOpInt _   _        _        = Nothing

mEvalExpRBool :: ExpR Bool -> TableroR -> Maybe Bool
mEvalExpRBool (Lit b)           t = Just b
mEvalExpRBool (PuedeMover d)    t = Just (puedeMover d t)
mEvalExpRBool NroBolitas        _ = Nothing
mEvalExpRBool HayBolitas        t = Just (hayBolitas t)
mEvalExpRBool (UnOp uop e)      t = mUOpBool uop (mEvalExpRBool e t)
mEvalExpRBool (BinOp bop e1 e2) t = mBOpBool bop (mEvalExpRBool e1 t) (mEvalExpRBool e2 t)

mUOpBool :: UOp -> Maybe Bool -> Maybe Bool
mUOpBool No (Just b) = Just (not b)
mUOpBool _  _        = Nothing

mBOpBool :: BOp -> Maybe Bool -> Maybe Bool -> Maybe Bool
mBOpBool YTambien (Just b) m = if b then m else Just False
mBOpBool OBien    (Just b) m = if b then Just True else m
mBOpBool _        _        _ = Nothing

-- Ejercicio vi
cantSacar :: ComandoR -> Int
cantSacar (Mover _)         = 0
cantSacar Poner             = 0
cantSacar Sacar             = 1
cantSacar NoOp              = 0
cantSacar (Repetir _ c)     = cantSacar c
cantSacar (Mientras _ c)    = cantSacar c
cantSacar (Secuencia c1 c2) = cantSacar c1 + cantSacar c2

-- Ejercicio vii
seqN :: Int -> ComandoR -> ComandoR
seqN 0 c = c
seqN n c = Secuencia c (seqN (n-1) c)

-- Ejercicio viii
repeat2Seq :: ComandoR -> ComandoR
repeat2Seq (Repetir (Lit n) c) = seqN n c
repeat2Seq c                   = c

-- Hacer 3 ejemplos de NExp

DEMOS
EJERCICIO EXTRA
Prop: ¿cantExtra . cfNE = const 0?
Dem: por ppio de ext.
¿para todo e. (cantExtra . cfNE) e = const 0 e?
por (.) y const
¿para todo e. cantExtra (cfNE e) = 0?
sea e' una NExp. Por ppio de ind. en la estructura de e':
Caso base 1, e' = Var x) ¿cantExtra (cfNE (Var x)) = 0?
Caso base 2, e' = NCte n) ¿cantExtra (cfNE (NCte n)) = 0?
Caso ind., e' = NBOp bop ne1 ne2) HI1) ¡cantExtra (cfNE ne1) = 0!
                                  HI2) ¡cantExtra (cfNE ne2) = 0!
                                  TI) ¿cantExtra (cfNE (NBOp bop ne1 ne2)) = 0?
Dem caso base 1:
cantExtra (cfNE (Var x))
= def cfNE.1
cantExtra (Var x)
= def cantExtra.1
0   -- VALE EL CASO
Dem caso base 2:
cantExtra (cfNE (NCte n))
= def cfNE.2
cantExtra (NCte n)
= def cantExtra.2
0   -- VALE EL CASO
Dem caso ind.:
Lado izq:
cantExtra (cfNE (NBOp bop ne1 ne2))
= def cfNE.3
cantExtra (cfBOp bop (cfNE ne1) (cfNE ne2))
= Lema cantExtra
cantExtra (cfNE ne1) + cantExtra (cfNE ne2)
= HI1, HI2
0 + 0
= aritm.
0   -- VALE EL CASO VALE LA PROPIEDAD

Lema cantExtra: ¿para todo e1. para todo e2. para todo o. cantExtra (cfBOp o e1 e2) = cantExtra e1 + cantExtra e2?
Dem: sea e1' y e2' dos NExp y o' un NBinOp:
Caso 1, e1' = Var x) ¿cantExtra (cfBOp o' (Var x) e2') = cantExtra (Var x) + cantExtra e2'?
Caso 2, e1' = NCte n) ¿cantExtra (cfBOp o' (NCte n) e2') = cantExtra (NCte n) + cantExtra e2'?
Caso 3, e1' = NBOp bop ne1 ne2) ¿cantExtra (cfBOp o' (NBOp bop ne1 ne2) e2') = cantExtra (NBOp bop ne1 ne2) + cantExtra e2'?
Dem caso 1:
Lado izq:
cantExtra (cfBOp o' (Var x) e2')
= def cfBOp.2
cantExtra (NBOp o' (Var x) e2')
= def cantExtra.3
unoSiPuedeOptimizar (Var x) e2' + cantExtra (Var x) + cantExtra e2'
= def unoSiPuedeOptimizar.2, def cantExtra.1
0 + 0 + cantExtra e2'
= artim.
cantExtra e2'
Lado der:
cantExtra (Var x) + cantExtra e2'
= def cantExtra.1
0 + cantExtra e2'
= aritm.
cantExtra e2'   -- VALE EL CASO
Dem caso 2:
Lado izq:
cantExtra (cfBOp o' (NCte n) e2')
= Lema cfBOp
cantExtra e2'
Lado der:
cantExtra (NCte n) + cantExtra e2'
= def cantExtra.2
0 + cantExtra e2'
= aritm.
cantExtra e2'   -- VALE EL CASO
Dem caso ind.:
Lado izq:
cantExtra (cfBOp o' (NBOp bop ne1 ne2) e2')
= def cfBOp.2
cantExtra (NBOp o' (NBOp bop ne1 ne2) e2')
= def cantExtra.3
unoSiPuedeOptimizar (NBOp bop ne1 ne2) e2' + cantExtra (NBOp bop ne1 ne2) + cantExtra e2'
= def unoSiPuedeOptimizar.2, cantExtra.3
0 + (unoSiPuedeOptimizar ne1 ne2 + cantExtra ne1 + cantExtra ne2) + cantExtra e2'
= aritm.
unoSiPuedeOptimizar ne1 ne2 + cantExtra ne1 + cantExtra ne2 + cantExtra e2'
Lado der:
cantExtra (NBOp bop ne1 ne2) + cantExtra e2'
= def cantExtra.3
unoSiPuedeOptimizar ne1 ne2 + cantExtra ne1 + cantExtra ne2 + cantExtra e2'   -- VALE EL CASO VALE LA PROPIEDAD

Lema cfBOp: ¿para todo e. para todo o. para todo n. cantExtra (cfBOp o (NCte n) e) = cantExtra e?
Dem: sea e' una NExp y o una BOp y m un Int:
Caso 1, e' = NCte n') ¿cantExtra (cfBOp o' (NCte m) (NCte n')) = cantExtra (NCte n')?
Caso 2, e' /= NCte n') ¿cantExtra (cfBOp o' (NCte m) e') = cantExtra e'?
Dem caso 1:
Lado izq:
cantExtra (cfBOp o' (NCte m) (NCte n'))
= def cfBOp.1
cantExtra (NCte (evalNBOp o' m n'))
= def cantExtra.2
0
Lado der:
cantExtra (NCte n')
= def cantExtra.2
0   -- VALE EL CASO
Dem caso 2:
Lado izq:
cantExtra (cfBOp o' (NCte m) e')
= def cfBOp.2
cantExtra (NBOp o' (NCte m) e')
= def cantExtra.3
unoSiPuedeOptimizar (NCte m) e' + cantExtra (NCte m) + cantExtra e'
= def unoSiPuedeOptimizar.2, def cantExtra.2
0 + 0 + cantExtra e'
= aritm.
cantExtra e'   -- VALE EL CASO VALE LA PROPIEDAD

EJERCICIO 1
Prop: ¿evalNExp . cfNExp = evalNExp?
Dem: por ppio de ext. y por (.)
¿para todo e. evalNExp (cfNExp e) = evalNExp e?
por ppio de ext.
¿para todo e. para todo m. evalNExp (cfNExp e) m = evalNExp e m?
sea e' una NExp y mem una Memoria. Por ppio de ind. en la estructura de e':
Caso base 1, e' = Var x) ¿evalNExp (cfNExp (Var x)) mem = evalNExp (Var x) mem?
Caso base 2, e' = NCte n) ¿evalNExp (cfNExp (NCte n)) mem = evalNExp (NCte n) mem?
Caso ind., e' = NBOp bop ne1 ne2) HI1) ¡evalNExp (cfNExp ne1) mem = evalNExp ne1 mem!
                                  HI2) ¡evalNExp (cfNExp ne2) mem = evalNExp ne2 mem!
                                  TI) ¿evalNExp (cfNExp (NBOp bop ne1 ne2)) mem = evalNExp (NBOp bop ne1 ne2) mem?
Dem caso base 1:
Lado izq:
evalNExp (cfNExp (Var x)) mem
= def cfNExp.1
evalNExp (Var x) mem   -- VALE EL CASO
Dem caso base 2:
Lado izq:
evalNExp (cfNExp (NCte n)) mem
= def cfNExp.2
evalNExp (NCte n) mem   -- VALE EL CASO
Dem caso ind.:
Lado izq:
evalNExp (cfNExp (NBOp bop ne1 ne2)) mem
= def cfNExp.3
evalNExp (cfBOp bop (cfNExp ne1) (cfNExp ne2)) mem
= Lema evalNExp
evalNBOp bop (evalNExp (cfNExp ne1) mem) (evalNExp (cfNExp ne2) mem)
= HI1, HI2
evalNBOp bop (evalNExp ne1 mem) (evalNExp ne2 mem)
Lado der:
evalNExp (NBOp bop ne1 ne2) mem
= def evalNExp.3
evalNBOp bop (evalNExp ne1 mem) (evalNExp ne2 mem)   -- VALE EL CASO VALE LA PROPIEDAD

Lema evalNExp: ¿para todo e1. para todo e2. para todo o. evalNExp (cfBOp o e1 e2) mem = evalNBOp o (evalNExp e1 mem) (evalNExp e2 mem)?
Dem: sean e1' y e2' dos NExp y o' un NBinOp:
Caso 1, e1' = NCte n y e2' = NCte m) ¿evalNExp (cfBOp o' (NCte n) (NCte m)) mem = evalNBOp o' (evalNExp (NCte n) mem) (evalNExp (NCte m) mem)?
Caso 2, e1' /= NCte n y e2' = NCte m) ¿evalNExp (cfBOp o' e1' (NCte m)) mem = evalNBOp o' (evalNExp e1' mem) (evalNExp (NCte m) mem)?
Caso 3, e1' /= NCte n y e2' /= NCte m) ¿evalNExp (cfBOp o' e1' e2') mem = evalNBOp o' (evalNExp e1' mem) (evalNExp e2' mem)?
Dem caso 1:
Lado izq:
evalNExp (cfBOp o' (NCte n) (NCte m)) mem
= def cfBOp.1
evalNExp (NCte (evalNBOp o' n m)) mem
= def evalNExp.2
evalNBOp o' n m
Lado der:
evalNBOp o' (evalNExp (NCte n) mem) (evalNExp (NCte m) mem)
= def evalNExp.2, def evalNExp.2
evalNBOp o' n m    -- VALE EL CASO
Dem caso base 2:
Lado izq:
evalNExp (cfBOp o' e1' (NCte m)) mem
= def cfBOp.2
evalNExp (NBOp (o' e1' (NCte m))) mem
= def evalNExp.3
evalNBOp o' (evalNExp e1' mem) (evalNExp (NCte m) mem)   -- VALE EL CASO
Dem caso 3:
Lado izq:
evalNExp (cfBOp o' e1' e2') mem
= def cfBOp.2
evalNExp (NBOp o' e1' e2') mem
= def evalNExp.3
evalNBOp o' (evalNExp e1' mem) (evalNExp e2' mem)   -- VALE EL CASO VALE LA PROPIEDAD


EJERCICIO 2
Prop: ¿evalBExp . cfBExp = evalBExp?
Dem: por ppio de ext. y por (.)
¿para todo e. evalBExp (cfBExp e) = evalBExp e?
por ppio de ext.
¿para todo e. para todo m. evalBExp (cfBExp e) m = evalBExp e m?
sea be' una BExp y mem una Memoria. Por ppio de ind. en la estructura de be':
Caso base 1, be' = BCte b) ¿evalBExp (cfBExp (BCte b)) mem = evalBExp (BCte b) mem?
Caso base 2, be' = ROp op e1 e2) ¿evalBExp (cfBExp (ROp op e1 e2)) mem = evalBExp (ROp op e1 e2) mem?
Caso ind. 1, be' = Not e') HI11) ¡evalBExp (cfBExp e') mem = evalBExp e' mem!
                           TI1) ¿evalBExp (cfBExp (Not e')) mem = evalBExp (Not e') mem?
Caso ind. 2, be' = And e1 e2) HI12) ¡evalBExp (cfBExp e1) mem = evalBExp e1 mem!
                              HI22) ¡evalBExp (cfBExp e2) mem = evalBExp e2 mem!
                              TI2) ¿evalBExp (cfBExp (And e1 e2)) mem = evalBExp (And e1 e2) mem?
Caso ind. 3, be' = Or e1 e2) HI13) ¡evalBExp (cfBExp e1) mem = evalBExp e1 mem!
                             HI23) ¡evalBExp (cfBExp e2) mem = evalBExp e2 mem!
                             TI3) ¿evalBExp (cfBExp (Or e1 e2)) mem = evalBExp (Or e1 e2) mem?
Dem caso base 1:
Lado izq:
evalBExp (cfBExp (BCte b)) mem
= def cfBExp.1
evalBExp (BCte b) mem   -- VALE EL CASO
Dem caso base 2:
Lado izq:
evalBExp (cfBExp (ROp op e1 e2)) mem
= def cfBExp.5
evalBExp (cfROp op (cfNExp e1) (cfNExp e2)) mem
= Lema cfROp
evalROp op (evalNExp (cfNExp e1) mem) (evalNExp (cfNExp e2) mem)
= prop. evalNExp . cfNExp = evalNExp, prop. evalNExp . cfNExp = evalNExp
evalROp op (evalNExp e1 mem) (evalNExp e2 mem)
Lado der:
evalBExp (ROp op e1 e2) mem
= def evalBExp.5
evalROp op (evalNExp e1 mem) (evalNExp e2 mem)   -- VALE EL CASO
Dem caso ind.1:
Lado izq:
evalBExp (cfBExp (Not e')) mem
= def cfBExp.2
evalBExp (cfNot (cfBExp e')) mem
= Lema cfNot
not (evalBExp (cfBExp e') mem)
Lado der:
evalBExp (Not e') mem
= def evalBExp.2
not (evalBExp e' mem)
= HI1
not (evalBExp (cfBExp e') mem)  -- VALE EL CASO
Dem caso ind. 2:
Lado izq:
evalBExp (cfBExp (And e1 e2)) mem
= def cfBExp.3
evalBExp (cfAnd (cfBExp e1) (cfBExp e2)) mem
= Lema cfAnd
evalBExp (cfBExp e1) mem && evalBExp (cfBExp e2) mem
= HI1, HI2
evalBExp e1 mem && evalBExp e2 mem
Lado der:
evalBExp (And e1 e2) mem
= def evalBExp.3
evalBExp e1 mem && evalBExp e2 mem   -- VALE EL CASO
Dem caso ind. 3:
Lado izq:
evalBExp (cfBExp (Or e1 e2)) mem
= def cfBExp.4
evalBExp (cfOr (cfBExp e1) (cfBExp e2)) mem
= Lema cfOr
evalBExp (cfBExp e1) mem || evalBExp (cfBExp e2) mem
= HI1, HI2
evalBExp e1 mem || evalBExp e2 mem
Lado der:
evalBExp (Or e1 e2) mem
= def evalBExp.4
evalBExp e1 mem || evalBExp e2 mem   -- VALE EL CASO VALE LA PROPIEDAD

Lema cfROp: ¿para todo e1. para todo e2. para todo o. evalBExp (cfROp o e1 e2) mem = evalROp o (evalNExp e1 mem) (evalNExp e2 mem)?
Dem: sean e1' y e2' NExp y op RelOp:
Caso 1, e1' /= NCte n) ¿evalBExp (cfROp op e1' e2') mem = evalROp op (evalNExp e1' mem) (evalNExp e2' mem)?
Caso 2, e1' = NCte n) ¿evalBExp (cfROp op (NCte n) e2') mem = evalROp op (evalNExp (NCte n) mem) (evalNExp e2' mem)?
   Subcaso 1, e2' /= NCte m) ¿evalBExp (cfROp op (NCte n) e2') mem = evalROp op (evalNExp (NCte n) mem) (evalNExp e2' mem)?
   Subcaso 2, e2' = NCte m) ¿evalBExp (cfROp op (NCte n) (NCte m)) mem = evalROp op (evalNExp (NCte n) mem) (evalNExp (NCte m) mem)?
Dem caso 1:
Lado izq:
evalBExp (cfROp op e1' e2') mem
= def cfROp.2
evalBExp (ROp op e1' e2') mem
= def evalBExp.5
evalROp op (evalNExp e1' mem) (evalNExp e2' mem)   -- VALE EL CASO
Dem caso 2, subcaso 1:
Lado izq:
evalBExp (cfROp op (NCte n) e2') mem
= def cfROp.2
evalBExp (ROp op (NCte n) e2') mem
= def evalBExp.5
evalROp op (evalNExp (NCte n) mem) (evalNExp e2' mem)   -- VALE EL CASO
Dem caso 2, subcaso 2:
Lado izq:
evalBExp (cfROp op (NCte n) (NCte m)) mem
= def cfROp.1
evalBExp (BCte (evalROp op n m)) mem
= def evalBExp.1
evalROp op n m
Lado der:
evalROp op (evalNExp (NCte n) mem) (evalNExp (NCte m) mem)
= def evalNExp.1, def evalNExp.1
evalROp op n m   -- VALE EL CASO VALE LA PROPIEDAD

Lema cfNot: ¿para todo e. para todo m. evalBExp (cfNot e) m = not (evalBExp e m)?
Dem: sea e' una BExp y mem una Memoria:
Caso 1, e' = BCte b) ¿evalBExp (cfNot (BCte b)) mem = not (evalBExp (BCte b) mem)?
Caso 2, e' /= BCte b) ¿evalBExp (cfNot e') mem = not (evalBExp e' mem)?
Dem caso 1:
Lado izq:
evalBExp (cfNot (BCte b)) mem
= def cfNot.1
evalBExp (BCte (not b)) mem
= def evalBExp.1
not b
Lado der:
not (evalBExp (BCte b) mem)
= def evalBExp.1
not b   -- VALE EL CASO
Dem caso 2:
Lado izq:
evalBExp (cfNot e') mem
= def cfNot.2
evalBExp (Not e') mem
= def evalBExp.2
not (evalBExp e' mem)   --VALE EL CASO VALE LA PROPIEDAD

Lema cfAnd: ¿para todo e1. para todo e2. para todo m. evalBExp (cfAnd e1 e2) m = evalBExp e1 m && evalBExp e2 m?
Dem: sean e1' y e2' dos BExp y mem una Memoria:
Caso 1, e1' = BCte b) ¿evalBExp (cfAnd (BCte b) e2') mem = evalBExp (BCte b) mem && evalBExp e2' mem?
Caso 2, e1' /= BCte b) ¿evalBExp (cfAnd e1' e2') mem = evalBExp e1' mem && evalBExp e2' mem?
Dem caso 1:
Lado izq:
evalBExp (cfAnd (BCte b) e2') mem
= def cfAnd.1
evalBExp (if b then e2' else BCte False) mem
= prop.if-then-else
if b then evalBExp e2' mem else evalBExp (BCte False) mem
= def evalBExp.1
if b then evalBExp e2' mem else False
= Lema if-and
b && evalBExp e2' mem
Lado der:
evalBExp (BCte b) mem && evalBExp e2' mem
= def evalBExp.1
b && evalBExp e2' mem   -- VALE EL CASO
Dem caso 2:
Lado izq:
evalBExp (cfAnd e1' e2') mem
= def cfAnd.2
evalBExp (And e1' e2') mem
= def evalBExp.3
evalBExp e1' mem && evalBExp e2' mem   -- VALE EL CASO VALE LA PROPIEDAD

Lema if-and: ¿para todo b1. para todo b2. if b1 then b2 else False = b1 && b2?
Dem: sea b1' y b2' dos Bool:
Caso 1, b1' = True) ¿if True then b2' else False = True && b2'?
Caso 2, b1' = False) ¿if False then b2' else False = False && b2'?
Dem caso 1:
Lado izq:
if True then b2' else False
= prop. if
b2'
Lado der:
True && b2'
= def &&.2
b2'   -- VALE EL CASO
Dem caso 2:
Lado izq:
if False then b2' else False
= prop. if
False
Lado der:
False && b2'
= def &&.1
False   -- VALE EL CASO VALE LA PROPIEDAD

Lema cfOr: ¿para todo e1. para todo e2. para todo m. evalBExp (cfOr e1 e2) m = evalBExp e1 m || evalBExp e2 m?
Dem: sean e1' y e2' dos BExp y mem una Memoria:
Caso 1, e1' = BCte b) ¿evalBExp (cfOr (BCte b) e2') mem = evalBExp (BCte b) mem || evalBExp e2' mem?
Caso 2, e1' /= BCte b) ¿evalBExp (cfOr e1' e2') mem = evalBExp e1' mem || evalBExp e2' mem?
Dem caso 1:
Lado izq:
evalBExp (cfOr (BCte b) e2') mem
= def cfOr.1
evalBExp (if b then BCte True else e2') mem
= prop. if-then-else
if b then evalBExp (BCte True) mem else evalBExp e2' mem
= def evalBExp.1
if b then True else evalBExp e2' mem
= Lema if-or
b || evalBExp e2' mem
Lado der:
evalBExp (BCte b) mem || evalBExp e2' mem
= def evalBExp.1
b || evalBExp e2' mem   -- VALE EL CASO
Dem caso 2:
evalBExp (cfOr e1' e2') mem
= def cfOr.2
evalBExp (Or e1' e2') mem
= def evalBExp.4
evalBExp e1' mem || evalBExp e2' mem   -- VALE EL CASO VALE LA PROPIEDAD

Lema if-or: ¿para todo b1. para todo b2. if b1 then True else b2 = b1 || b2?
Dem: sean b1' y b2' dos Bool:
Caso 1, b1' = True) ¿if True then True else b2' = True || b2'?
Caso 2, b1' = False) ¿if False then True else b2' = False || b2'?
Dem caso 1:
Lado izq:
if True then True else b2'
= prop. if
True
Lado der:
True || b2'
= def ||.1
True   -- VALE EL CASO
Dem caso 2:
Lado izq:
if False then True else b2'
= prop. if
b2'
Lado der:
False || b2'
= def ||.2
b2'   -- VALE EL CASO VALE LA PROPIEDAD
