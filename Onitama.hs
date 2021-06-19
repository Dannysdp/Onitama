data Pieza = Peon Int Int | Maestro Int Int | Vacio Int Int deriving (Eq,Show)

data Place = Move Int Int deriving (Eq,Show)

data OnitamaGame = OnitamaGame Tablero [OnitamaCard] [OnitamaCard] OnitamaCard OnitamaPlayer  -- tablero actual, cartas1, cartas2, carta extra y juegador

data OnitamaAction = OnitamaAction (Place,Place)
-- hay que ponerle la carta q utiliza esta accion y la pieza q modifica, pero mepa q eso es "Peleable"

-- a1 a2 ra a3 a4 | 0,0 0,1 0,2 0,3 0,4
--  -  -  -  -  - | 1,0 1,1 1,2 1,3 1,4 
--  -  -  -  -  - | 2,0 2,1 2,2 2,3 2,4
--  -  -  -  -  - | 3,0 3,1 3,2 3,3 3,4
-- r1 r2 rr r3 r4 | 4,0 4,1 4,2 4,3 4,4
-- x = vertial y = horizontal 
{-
 [(Movimiento,Movimiento)] 
 !! 0 = (Move,Move)
 !! 1 = carta1 (un jugador)
 !! 2 = carta2 (un jugador)
 !! 3 = carta3 (otro jugador) 
 !! 4 = carta4 (otro jugador)
 !! 5 = carta5 (Extra)
d7 a d6
-}

data Tablero = Tablero [Pieza]
--crear una funcion maketable que usando onitamaGame arma el tablero
data OnitamaCard = Tiger | Dragon | Frog | Rabbit | Crab | Elephant | Goose | Rooster | Monkey | Mantis | Horse | Ox | Crane | Boar | Eel | Cobra 
-- aca ponemos todas las cartas (tiger,oz,dragon)

cartaATupla :: OnitamaCard -> [(Int,Int)]
cartaATupla (Tiger) = [(-1,0),(2,0)] -- [(x,y)] x,y son posiciones de la matriz tablero
cartaATupla (Dragon) = [(-1,1),(-1,-1),(1,2),(1,-2)]
cartaATupla (Frog) = [(-1,-1),(0,-2),(1,1)]
cartaATupla (Rabbit) = [(1,-1),(-1,1)]
cartaATupla (Crab) = [(0,-2),(-1,0),(0,2)]
cartaATupla (Elephant) = [(-1,-1),(0,-1),(1,1)] -- sin terminar 

data OnitamaPlayer = RedPlayer | BluePlayer deriving(Eq, Show, Enum)

data GameResult p = Winner p | Loser p | Drawderiving(Eq, Show)
-- : El estado inicial del juego de Onitama. Esto incluye elorden en el cual están barajadas las cartas del mazo
beginning :: [OnitamaCard] -> OnitamaGame -- yo doy barajas

beginning :: OnitamaGame
beginning = Tablero [(Pieza Vacio x y) | x <- [1..3] y <- [0..4]] ++ [(Pieza BluePlayer x y) | x <- 1 y <- [0..4]] ++ [(Pieza RedPlayer x y) | x <- 4 y <- [0..4]]

--Esta función determina a cuál jugador le toca mover,dado un estado de juego.
activePlayer :: OnitamaGame -> OnitamaPlayer
activePlayer (OnitamaGame  _ _ _ _ jugador) = jugador

--La lista debe incluir una y solo una tupla para cada jugador. Si el jugador está activo, la lista asociada debe incluir todos sus posiblesmovimientos para el estado de juego dado. Sino la lista debe estar vacía.
actions :: OnitamaGame -> [(OnitamaPlayer, [OnitamaAction])]
actions tablero@(Tablero lista _) = [actionsRed] ++ [actionsBlue] where
 actionsRed = --definir el RedPlayer, zip con una funcion listaInsertar y otra que
                -- vea las casillas vacias?? 


-- Esta función aplica una acción sobre un estado de juego dado, y retorna el estado resultante. Se debe levantar un error si eljugador dado no es el jugador activo, si el juego está terminado, o si la acción no es realizable.
-- next :: OnitamaGame -> (OnitamaPlayer, OnitamaAction) -> OnitamaGame
next :: OnitamaGame -> OnitamaPlayer -> OnitamaAction -> OnitamaGame
--next :: estadoActual -> JugadorSiguiente (el que va a jugar ahora, NO el de estado actual), un movimiento -------> DEVUELVE UN estadoSiguiente.
next gameActual jugador accion
 | activePlayer OnitamaGame != jugador = error "No puede jugar dos veces seguidas"
 | (OnitamaGame tablero _ _ _ _) jugadorAct accion = if (actualizoTablero tablero accion) == Nothing then error "No se puede realizar ese movimiento!" else OnitamaGame (actualizoTablero tablero accion) jugadorAct accion
 otherwise = error "No has introducido un onitamaGame. Su llamado a esta función es imposible de procesar."


--Si el juego está terminado retorna el resul-tado de juego para cada jugador. Si el juego no está terminado, se debe retornar una lista vacía.
result :: OnitamaGame -> [GameResult OnitamaPlayer]

--Retorna el puntaje para todos los jugadoresen el estado de juego dado. Esto es independiente de si el juego está terminado o no.
-- score :: OnitamaGame -> [(OnitamaPlayer, Double)]

-- Convierte el estado de juego a un texto que puede ser impresoen la consola para mostrar el tablero y demás información de la partida.
-- showGame :: OnitamaGame -> String

--Convierte una acción a un texto que puede ser impreso enla consola para mostrarla.
-- showAction :: OnitamaAction -> String

--Obtiene una acción a partir de un texto que puede habersido introducido por el usuario en la consola.
-- readAction :: String -> OnitamaAction