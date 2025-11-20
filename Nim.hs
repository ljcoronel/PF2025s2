data Nim = Empty | Heap Int Nim
     deriving Show
type Move = (Int,Int)
data GameTree = Nil | Node (Move, GameTree) GameTree
     deriving Show

nimA = Heap 3 Empty
nimB = Heap 3 (Heap 2 (Heap 1 Empty))
nimC = Heap 2 (Heap 1 Empty)
gt = Node ((0,1), Node ((0,1), Node ((1,1), Nil) Nil) (Node ((1,1), Node ((0,1), Nil) Nil) Nil)) (Node ((0,2), Node ((1,1), Nil) Nil) (Node ((1,1), Node ((0,1), Node ((0,1), Nil) Nil) (Node ((0,2), Nil) Nil)) Nil))

foldNim :: b -> (Int -> b -> b) -> Nim -> b
foldNim z f Empty        = z
foldNim z f (Heap n nim) = f n (foldNim z f nim)

recNim :: b -> (Int -> Nim -> b -> b) -> Nim -> b
recNim z f Empty        = z
recNim z f (Heap n nim) = f n nim (recNim z f nim)

foldGame :: b -> (Move -> b -> b -> b) -> GameTree -> b
foldGame z f Nil                  = z
foldGame z f (Node (move, tC) tP) = f move (foldGame z f tC) (foldGame z f tP)

recGame :: b -> (Move -> GameTree -> GameTree -> b -> b -> b) -> GameTree -> b
recGame z f Nil                  = z
recGame z f (Node (move, tC) tP) = f move tC tP (recGame z f tC) (recGame z f tP)

heaps :: Nim -> Int
heaps = foldNim 0 (\_ n -> 1 + n)

chips :: Nim -> Int
chips = foldNim 0 (\n m -> n + m)

maxHeap :: Nim -> Int
maxHeap = foldNim 0 (\n m -> n `max` m)

alongside  ::  Nim  ->  Nim  ->  Nim
alongside = foldNim id (\n h -> \nim -> Heap n (h nim))

gameHeight  ::  GameTree  ->  Int
gameHeight = foldGame 0 (\_ n _ -> 1 + n)

branches  ::  GameTree  ->  [[Move]]
branches = foldGame [] (\move mss1 mss2 -> (if null mss1 then [move] : mss1 else map (move :) mss1) ++ mss2)

turn :: Nim -> Move -> Maybe Nim
turn Empty        _     = Nothing
turn (Heap n nim) (0,k) = if n >= k then Just (Heap (n-k) nim) else Nothing
turn (Heap n nim) (i,k) = case turn nim (i-1,k) of
                               Nothing   -> Nothing
                               Just nim' -> Just (Heap n nim')

moves :: Nim -> [Move]
moves Empty        = []
moves (Heap n nim) = jugadaActual n ++ map sumarJugada (moves nim)

jugadaActual :: Int -> [Move]
jugadaActual 0 = []
jugadaActual n = (0,n) : jugadaActual (n-1)

sumarJugada :: Move -> Move
sumarJugada (i,k) = (i+1,k)

solve :: Nim -> GameTree
solve nim = genGT nim (moves nim)

genGT :: Nim -> [Move] -> GameTree
genGT Empty _      = Nil
genGT _     []     = Nil
genGT nim   (m:ms) = let nim' = fromJust (turn nim m)
                      in Node (m,genGT nim' (moves nim')) (genGT nim ms)

fromJust :: Maybe a -> a
fromJust Nothing  = error "boom"
fromJust (Just x) = x