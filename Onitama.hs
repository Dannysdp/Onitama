data Pieza = Piece Maybe Int Maybe Int  deriving (Eq,Show)

data Place = Move Int Int deriving (Eq,Show)

data OnitamaGame = OnitamaGame Tablero [OnitamaCard] [OnitamaCard] OnitamaCard OnitamaPlayer  -- tablero actual, cartas1, cartas2, carta extra y juegador

data OnitamaAction = OnitamaAction (Place,Place)

a1 a2 ra a3 a4
 -  -  -  -  -
 -  -  -  -  -
 -  -  -  -  -
r1 r2 rr r3 r4 

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

data Tablero = Tablero [[Piece]]
--crear una funcion maketable que usando onitamaGame arma el tablero
data OnitamaCard = Tiger 
-- aca ponemos todas las cartas (tiger,oz,dragon)

cartaATupla :: OnitamaCard -> [(Int,Int)]
cartaATupla (Tiger) = [(2,2),(3,3),(4,4)] -- [(x,y)] x,y son posiciones de la matriz tablero


data OnitamaPlayer = RedPlayer | BluePlayer deriving(Eq, Show, Enum)

data GameResult p = Winner p | Loser p | Drawderiving(Eq, Show)
-- : El estado inicial del juego de Onitama. Esto incluye elorden en el cual están barajadas las cartas del mazo
beginning :: [OnitamaCard] -> OnitamaGame -- yo doy barajas

--Esta función determina a cuál jugador le toca mover,dado un estado de juego.
activePlayer :: OnitamaGame -> OnitamaPlayer

--La lista debe incluir una y solo una tupla para cada jugador. Si el jugador está activo, la lista asociada debe incluir todos sus posiblesmovimientos para el estado de juego dado. Sino la lista debe estar vacía.
actions :: OnitamaGame -> [(OnitamaPlayer, [OnitamaAction])]

-- Esta función aplica una acción sobre un estado de juego dado, y retorna el estado resultante. Se debe levantar un error si eljugador dado no es el jugador activo, si el juego está terminado, o si la acción no es realizable.
-- next :: OnitamaGame -> (OnitamaPlayer, OnitamaAction) -> OnitamaGame
next :: OnitamaGame -> OnitamaPlayer -> OnitamaAction -> OnitamaGame

--Si el juego está terminado retorna el resul-tado de juego para cada jugador. Si el juego no está terminado, se debe retornar una lista vacía.
result :: OnitamaGame -> [GameResult OnitamaPlayer]

--Retorna el puntaje para todos los jugadoresen el estado de juego dado. Esto es independiente de si el juego está terminado o no.
score :: OnitamaGame -> [(OnitamaPlayer, Double)]

-- Convierte el estado de juego a un texto que puede ser impresoen la consola para mostrar el tablero y demás información de la partida.
showGame :: OnitamaGame -> String

--Convierte una acción a un texto que puede ser impreso enla consola para mostrarla.
showAction :: OnitamaAction -> String

--Obtiene una acción a partir de un texto que puede habersido introducido por el usuario en la consola.
readAction :: String -> OnitamaAction