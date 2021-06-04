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
jugar (p:ps) (1, c)
    | c' > 0 = c' : ps
    | c' == 0 = ps
    where c' = p - c
jugar (p:ps) (i, c) = p : jugar ps (i-1, c)


-- Recibe una posición p y devuelve el conjunto de jugadas válidas a partir de p.
-- Armo las tuplas de jugadas con la cantidad de mayor a menor porque así hago
-- un poquito más eficiente las otras funciones.
posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas [] = []
posiblesJugadas p =
    posiblesJugadas (init p) ++ zip (repeat (length p)) [ultimoP,ultimoP-1..1]
    where ultimoP = last p


-- Decide si una posición p es ganadora.
esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora p = length (take 1 (jugadasGanadoras p)) > 0


-- Recibe una posición ganadora p y devuelve una jugada que dejaría al rival en
-- una posición no ganadora.
jugadaGanadora :: Posicion -> Jugada
jugadaGanadora p = head (take 1 (jugadasGanadoras p))


-- Recibe una posición p (no necesariamente ganadora) y devuelve la cantidad de
-- jugadas ganadoras partiendo de p.
numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras p = length (jugadasGanadoras p)


-- Funciones auxiliares


-- Recibe una posición p (no necesariamente ganadora) y devuelve todas las
-- jugadas que dejaría al rival en una posición no ganadora.
jugadasGanadoras :: Posicion -> [Jugada]
jugadasGanadoras p = filtrarJugadasGanadoras p (posiblesJugadas p)


-- Recibe una posición p (no necesariamente ganadora), un conjunto de jugadas
-- válidas js, y devuelve todas las jugadas que dejaría al rival en una
-- posición no ganadora.
filtrarJugadasGanadoras :: Posicion -> [Jugada] -> [Jugada]
filtrarJugadasGanadoras _ [] = []
filtrarJugadasGanadoras p (j:js)
    | not (esPosicionGanadora (jugar p j)) = j : filtrarJugadasGanadoras p js
    | otherwise = filtrarJugadasGanadoras p js
