-- Taller de Álgebra 1: Primer cuatrimestre 2021
-- Trabajo práctico 2: El juego de Pop-it
-- Nombre: Jonathan Bekenstein
-- DNI: 32757221
-- Libreta: 348/11


type Posicion = [Int]
type Jugada = (Int, Int)


-- Recibe una posición p, una jugada válida j y devuelve la posición obtenida
-- al realizar dicha jugada.
jugar :: Posicion -> Jugada -> Posicion
jugar (p:ps) (1, c)
    | c' > 0 = c' : ps
    | c' == 0 = ps
    where c' = p - c
jugar (p:ps) (i, c) = p : jugar ps (i-1, c)


-- Recibe una posición p y devuelve el conjunto de jugadas válidas a partir de p.
posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas [] = []
posiblesJugadas p = posiblesJugadas (init p) ++ zip (repeat (length p)) [ultimoP, ultimoP-1 .. 1]
    where ultimoP = last p


-- Decide si una posición p es ganadora.
esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora p = length (take 1 (filtrarJugadasGanadoras p (posiblesJugadas p))) > 0


-- Recibe una posición ganadora p y devuelve una jugada que dejaría al rival en
-- una posición no ganadora.
jugadaGanadora :: Posicion -> Jugada
jugadaGanadora p = head (take 1 (filtrarJugadasGanadoras p (posiblesJugadas p)))


-- Recibe una posición p (no necesariamente ganadora) y devuelve la cantidad de
-- jugadas ganadoras partiendo de p.
numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras p = length (filtrarJugadasGanadoras p (posiblesJugadas p))


-- Funciones auxiliares

-- Recibe una posición p, un listado de jugadas posibles y devuelve únicamente
-- las jugadas que resultan ganadoras.
filtrarJugadasGanadoras :: Posicion -> [Jugada] -> [Jugada]
filtrarJugadasGanadoras _ [] = []
filtrarJugadasGanadoras p (j:js)
    | not (esPosicionGanadora (jugar p j)) = j : filtrarJugadasGanadoras p js
    | otherwise = filtrarJugadasGanadoras p js


-- Testea todos los casos de prueba provistos en el enunciado del TP.
test :: Bool
test = jugar [3,3,3] (1,3) == [3,3]
    && jugar [3,3,3] (2,1) == [3,2,3]
    && jugar [5,4,3,2,1] (2,3) == [5,1,3,2,1]
    && posiblesJugadas [] == []
    && posiblesJugadas [1] == [(1,1)]
    && posiblesJugadas [1,2,2] == [(1,1),(2,2),(2,1),(3,2),(3,1)]
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
