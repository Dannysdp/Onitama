import Data.Maybe (fromMaybe)

data Pieza = Peon OnitamaPlayer | Maestro OnitamaPlayer | Vacio deriving (Eq,Show)

-- data Move = Move (Integer, Integer) deriving (Eq,Show)

data OnitamaGame = OnitamaGame Tablero [OnitamaCard] [OnitamaCard] [OnitamaCard] OnitamaPlayer deriving(Show)  -- tablero actual, cartas1, cartas2, carta extra y juegador

data OnitamaAction = OnitamaAction (Integer, Integer) (Integer, Integer) deriving(Show)

data Tablero = Tablero [Pieza] deriving(Show)

data OnitamaCard = Tiger | Dragon | Frog | Rabbit | Crab | Elephant | Goose | Rooster | Monkey | Mantis | Horse | Ox | Crane | Boar | Eel | Cobra deriving(Show)

data OnitamaPlayer = RedPlayer | BluePlayer deriving(Eq, Show, Enum)

data GameResult p = Winner p | Loser p | Draw deriving(Eq, Show)

--                  cordenadas x y           pos lista 
-- r1 r2 rr r3 r4 | 0,4 1,4 2,4 3,4 4,4  -> 20 21 22 23 24 
--  -  -  -  -  - | 0,3 1,3 2,3 3,3 4,3  -> 15 16 17 18 19 
--  -  -  -  -  - | 0,2 1,2 2,2 3,2 4,2  -> 10 11 12 13 14 
--  -  -  -  -  - | 0,1 1,1 2,1 3,1 4,1  -> 5  6  7  8  9 
-- a1 a2 ra a3 a4 | 0,0 1,0 2,0 3,0 4,0  -> 0  1  2  3  4 
-- x = horizontal y = veritcal 
-- convertir de pos lista a cordenadas = x + y*5

beginning :: [OnitamaCard] -> OnitamaGame -- yo doy barajas
beginning baraja = (OnitamaGame tableroInicial (fst cartas1) (fst cartas2) (fst cartaE) RedPlayer)
    where 
        cartas1 = (splitAt 2 baraja)
        cartas2 = (splitAt 2 (snd cartas1))
        cartaE = (splitAt 1 (snd cartas2))

activePlayer :: OnitamaGame -> OnitamaPlayer
activePlayer (OnitamaGame  _ _ _ _ jugador) = jugador

activePieza :: OnitamaGame -> [Pieza]
activePieza (OnitamaGame  (Tablero []) _ _ _ _) = []
activePieza game@(OnitamaGame  (Tablero (piez:lista)) _ _ _ _) = [piez] ++ activePieza game


--La lista debe incluir una y solo una tupla para cada jugador. Si el jugador está activo, la lista asociada debe incluir todos sus posibles movimientos para el estado de juego dado. Sino la lista debe estar vacía.
actions :: OnitamaGame -> [(OnitamaPlayer, [OnitamaAction])]
actions game@(OnitamaGame tablero cartasR cartasA cartasE jugador) = [(RedPlayer, if jugador == RedPlayer then actionsRed else []), (BluePlayer, if jugador == BluePlayer then actionsBlue else [])]
    where actionsRed = recorrerTablero game 0
          actionsBlue = recorrerTablero game 0

recorrerTablero :: OnitamaGame -> Integer -> [OnitamaAction]
recorrerTablero (OnitamaGame (Tablero []) cartasR cartasA cartasE jugador) _  = []
recorrerTablero game@(OnitamaGame t@(Tablero tablero) cartasR cartasA cartasE jugador) pos
 |pos == 24 = []
 |piezaAJugador (tablero!!(fromIntegral pos)) == (Maybe just jugador | Nothing) = crearMov (tablero!!(fromIntegral pos)) t pos (cartasJugador game) ++ recorrerTablero game (pos+1)
 |otherwise = recorrerTablero game (pos+1)

crearMov :: Pieza -> Tablero -> Integer -> [OnitamaCard] -> [OnitamaAction]
crearMov _ _ _ [] = []
crearMov pieza t@(Tablero tablero) pos (mov:cartas) = if (tablero!!(fromIntegral pos)) /= pieza then movimientos (posACord pos) (cartaATupla mov) ++ crearMov pieza t pos cartas else []

movimientos (x,y) [] = []
movimientos (x,y) ((a,b):lista) = [(OnitamaAction (x,y) (x+a,y+b))] ++ movimientos (x,y) lista


{-
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
-}


------------------- AUXILIARES -------------------
--cordenada a posicion
cordAPos :: (Integer, Integer) -> Integer
cordAPos (x, y) = x + y * 5
--Posicion a cordenada 
posACord :: Integer -> (Integer, Integer)
posACord pos = (pos - 5 * (div pos 5), div pos 5)

-- TODO invertir x con y 
-- [(x,y)] x,y son posiciones de la matriz tablero
cartaATupla :: OnitamaCard -> [(Integer,Integer)]
cartaATupla (Tiger) = [(-1,0),(2,0)] 
cartaATupla (Dragon) = [(-1,1),(-1,-1),(1,2),(1,-2)]
cartaATupla (Frog) = [(-1,-1),(0,-2),(1,1)]
cartaATupla (Rabbit) = [(1,-1),(-1,1)]
cartaATupla (Crab) = [(0,-2),(-1,0),(0,2)]
cartaATupla (Elephant) = [(-1,-1),(0,-1),(1,1),(0,1)] 
cartaATupla (Goose) = [(-1,-1),(0,-1),(1,1),(1,1)] 
cartaATupla (Rooster) = [(1,-1),(0,-1),(1,1),(0,1)] 
cartaATupla (Monkey) = [(-1,-1),(1,-1),(-1,1),(1,1)] 
cartaATupla (Mantis) = [(-1,-1),(-1,1),(1,0)] 
cartaATupla (Horse) = [(0,-1),(-1,0),(1,0)] 
cartaATupla (Ox) = [(0,1),(-1,0),(1,0)] 
cartaATupla (Crane) = [(1,-1),(-1,0),(1,1)] 
cartaATupla (Boar) = [(0,-1),(-1,0),(0,1)] 
cartaATupla (Eel) = [(-1,-1),(1,-1),(0,1)] 
cartaATupla (Cobra) = [(1,-1),(1,1),(0,-1)] 

piezaAJugador :: Pieza -> Maybe OnitamaPlayer
piezaAJugador (Peon jugador) = Just jugador 
piezaAJugador (Maestro jugador) = Just jugador 
piezaAJugador (Vacio) = Nothing 


tableroInicial = (Tablero ((replicate 2 (Peon RedPlayer)) ++ [(Maestro RedPlayer)] ++ (replicate 2 (Peon RedPlayer)) ++ (replicate 15 Vacio) ++ (replicate 2 (Peon BluePlayer)) ++ [(Maestro BluePlayer)] ++ (replicate 2 (Peon BluePlayer))))

cartasJugador (OnitamaGame tablero cartasR cartasA cartasE jugador) = if jugador == RedPlayer then cartasR else cartasA 