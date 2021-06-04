-- Taller de Álgebra 1: Primer cuatrimestre 2021
-- Trabajo práctico 2: El juego de Pop-it
-- Nombre: Jonathan Bekenstein
-- DNI: 32757221
-- Libreta: 348/11


type Posicion = [Int]
type Jugada = (Int,Int)


-- Recibe una posición p, una jugada válida j y devuelve la posición obtenida
-- al realizar dicha jugada.
jugar :: Posicion -> Jugada -> Posicion
jugar (p:ps) (1,c)
    | c' > 0 = c' : ps
    | c' == 0 = ps
    where c' = p - c
jugar (p:ps) (i,c) = p : jugar ps (i-1,c)


-- Recibe una posición p y devuelve el conjunto de jugadas válidas a partir de p.
posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas [] = []
posiblesJugadas p = posiblesJugadas (init p) ++ zip (repeat (length p)) [1..(last p)]


-- Decide si una posición p es ganadora.
esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora p = length (take 1 (jugadasGanadoras p)) > 0


-- Recibe una posición ganadora p y devuelve una jugada que dejaría al rival en
-- una posición no ganadora.
jugadaGanadora :: Posicion -> Jugada
jugadaGanadora p = head (take 1 (jugadasGanadoras p))


-- Recibe una posición p (no necesariamente ganadora) y devuelve la cantidad de
-- jugadas ganadoras partiendo de p.
-- Está roto porque usa la versión optimizada de jugadasGanadoras que descarta las posiciones repetidas.
numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras p = length (jugadasGanadoras p)


-- Funciones auxiliares --------------------------------------------------------


jugarNormalizado :: Posicion -> Jugada -> Posicion
jugarNormalizado p j = ordenar (jugar p j)

ordenarJugadas :: [Jugada] -> [Jugada]
ordenarJugadas [] = []
ordenarJugadas js = jMax : ordenarJugadas (quitar jMax js)
    where jMax = jugadaMaxima js

jugadaMaxima :: [Jugada] -> Jugada
jugadaMaxima [j] = j
jugadaMaxima (j:js)
    | snd j >= snd (jugadaMaxima js) = j
    | otherwise = jugadaMaxima js

jugadasGanadoras :: Posicion -> [Jugada]
jugadasGanadoras p = filtrarJugadasGanadoras p [] (ordenarJugadas (posiblesJugadas p))

jugadasGanadorasCache :: Posicion -> [Posicion] -> [Jugada]
jugadasGanadorasCache p cache = filtrarJugadasGanadoras p cache (ordenarJugadas (posiblesJugadas p))

esPosicionGanadoraConCache :: Posicion -> [Posicion] -> Bool
esPosicionGanadoraConCache p cache = length (take 1 (jugadasGanadorasCache p cache)) > 0

filtrarJugadasGanadoras :: Posicion -> [Posicion] -> [Jugada] -> [Jugada]
filtrarJugadasGanadoras _ _ [] = []
filtrarJugadasGanadoras p cache (j:js)
    | proximaP `elem` cache = filtrarJugadasGanadoras p cache js
    | not (esPosicionGanadoraConCache proximaP cache) = j : filtrarJugadasGanadoras p (proximaP:cache) js
    | otherwise = filtrarJugadasGanadoras p (proximaP:cache) js
    where
        proximaP = jugarNormalizado p j

quitar :: Eq a => a -> [a] -> [a]
quitar _ [] = []
quitar n (x:xs)
    | n == x = xs
    | otherwise = x : quitar n xs

ordenar :: (Eq a, Ord a) => [a] -> [a]
ordenar [] = []
ordenar l = m : ordenar (quitar m l)
    where m = minimum l

-- Testea todos los casos de prueba provistos en el enunciado del TP.

test :: Bool
test = jugar [3,3,3] (1,3) == [3,3]
    && jugar [3,3,3] (2,1) == [3,2,3]
    && jugar [5,4,3,2,1] (2,3) == [5,1,3,2,1]
    && posiblesJugadas [] == []
    && posiblesJugadas [1] == [(1,1)]
    && posiblesJugadas [1,2,2] == [(1,1),(2,1),(2,2),(3,1),(3,2)]
    && esPosicionGanadora [] == False
    && esPosicionGanadora [1] == True
    && esPosicionGanadora [1,2] == True
    && esPosicionGanadora [1,2,3] == False
    && esPosicionGanadora [1,2,3,4] == True
    && esPosicionGanadora [1,1] == False
    && esPosicionGanadora [1,2,2] == True
    && jugadaGanadora [1,2,3,4] == (4,4)
    && (jugadaGanadora [1,1,1] == (1,1) || jugadaGanadora [1,1,1] == (2,1) || jugadaGanadora [1,1,1] == (3,1))
    && numeroDeJugadasGanadoras [] == 0
    && numeroDeJugadasGanadoras [1] == 1
    && numeroDeJugadasGanadoras [1,2,3,4] == 1
    && numeroDeJugadasGanadoras [1,1,1] == 3
