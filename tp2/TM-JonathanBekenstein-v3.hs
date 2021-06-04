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
-- Armo las tuplas de jugadas con la cantidad de mayor a menor porque así hago
-- un poquito más eficiente las otras funciones.
posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas [] = []
posiblesJugadas p = posiblesJugadas (cuerpo p)
                    ++ zipi (repetir (longitud p)) [ultimoP, ultimoP-1 ..1]
                    where ultimoP = ultimo p


-- Decide si una posición p es ganadora.
esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora p = longitud (tomar 1 (filtrarJugadasGanadoras p (posiblesJugadas p))) > 0


-- Recibe una posición ganadora p y devuelve una jugada que dejaría al rival en
-- una posición no ganadora.
jugadaGanadora :: Posicion -> Jugada
jugadaGanadora p = head (tomar 1 (filtrarJugadasGanadoras p (posiblesJugadas p)))


-- Recibe una posición p (no necesariamente ganadora) y devuelve la cantidad de
-- jugadas ganadoras partiendo de p.
numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras p = longitud (filtrarJugadasGanadoras p (posiblesJugadas p))


-- Funciones auxiliares


-- Recibe una posición p, un listado de jugadas posibles y devuelve únicamente
-- las jugadas que resultan ganadoras.
filtrarJugadasGanadoras :: Posicion -> [Jugada] -> [Jugada]
filtrarJugadasGanadoras _ [] = []
filtrarJugadasGanadoras p (j:js)
    | not (esPosicionGanadora (jugar p j)) = j : filtrarJugadasGanadoras p js
    | otherwise = filtrarJugadasGanadoras p js


-- Reimplementación de las funciones del preludio que usé.

-- head
cabeza :: [a] -> a
cabeza [x] = x

-- last
ultimo :: [a] -> a
ultimo [x] = x
ultimo (x:xs) = ultimo xs

-- init
cuerpo :: [a] -> [a]
cuerpo [x] = []
cuerpo (x:xs) = x : cuerpo xs

-- length
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- repeat
repetir :: a -> [a]
repetir n = n : repetir n

-- zip
zipi :: [a] -> [b] -> [(a,b)]
zipi [] _ = []
zipi _ [] = []
zipi (x:xs) (y:ys) = (x,y) : zipi xs ys

-- take
tomar :: Int -> [a] -> [a]
tomar 0 _ = []
tomar _ [] = []
tomar n (x:xs) = x : tomar (n-1) xs


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
