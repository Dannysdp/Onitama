{- Tak ------------------------------------------------------------------------------------------
 
Plantilla de código para el proyecto del curso de 2021 de _Programación Funcional_ para las carreras 
de Ingeniería y Licenciatura en Informática de la FIT (UCU).
Los docentes no garantizan que este código esté libre de errores. De encontrar problemas, por favor
reportarlos a la cátedra.

Leonardo Val, Ignacio Pacheco.
-}
module Onitama where

import Data.Maybe (fromJust, listToMaybe)
import Data.List (elemIndex, sort)
import System.Random
import Data.Maybe 

data Pieza = Peon OnitamaPlayer | Maestro OnitamaPlayer | Vacio deriving (Eq,Show)

data OnitamaGame = OnitamaGame Tablero [OnitamaCard] [OnitamaCard] [OnitamaCard] OnitamaPlayer deriving(Show)  -- tablero actual, cartas1, cartas2, carta extra y juegador
-- donde estoy la carta que uso y donde voy.
data OnitamaAction = OnitamaAction (Integer, Integer) OnitamaCard (Integer, Integer) deriving(Eq, Show)

data OnitamaCard = Tiger | Dragon | Frog | Rabbit | Crab | Elephant | Goose | Rooster | Monkey | Mantis | Horse | Ox | Crane | Boar | Eel | Cobra deriving(Show, Eq)

data Tablero = Tablero [Pieza] deriving(Show)

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


{- Es posible que el paquete `System.Random` no esté disponible si se instaló el core de la Haskell 
Platform en el sistema. Para instalarlo, ejecutar los siguientes comandos:

> cabal update
> cabal install --lib random


La herramienta `cabal` es un manejador de paquetes usado por la plataforma Haskell. Debería estar 
disponible junto con el `ghci`.

-}

{-- Lógica de juego --------------------------------------------------------------------------------

Funciones de marca sin ninguna implementación útil. Reemplazar por el código apropiado o por imports
a los módulos necesarios.
-}

beginning :: [OnitamaCard] -> OnitamaGame -- yo doy barajas
beginning baraja = (OnitamaGame tableroInicial (fst cartas1) (fst cartas2) (fst cartaE) RedPlayer)
    where 
        cartas1 = (splitAt 2 baraja)
        cartas2 = (splitAt 2 (snd cartas1))
        cartaE = (splitAt 1 (snd cartas2))

activePlayer :: OnitamaGame -> OnitamaPlayer
activePlayer (OnitamaGame  _ _ _ _ jugador) = jugador

--La lista debe incluir una y solo una tupla para cada jugador. Si el jugador está activo, la lista asociada debe incluir todos sus posibles movimientos para el estado de juego dado. Sino la lista debe estar vacía.
actions :: OnitamaGame -> [(OnitamaPlayer, [OnitamaAction])]
actions game@(OnitamaGame tablero cartasR cartasA cartasE jugador) = [(RedPlayer, if jugador == RedPlayer then actionsRed else []), (BluePlayer, if jugador == BluePlayer then actionsBlue else [])]
    where actionsRed = recorrerTablero game 0 crearMov
          actionsBlue = recorrerTablero game 0 crearMov

recorrerTablero :: OnitamaGame -> Integer -> (a -> Integer -> [b]) -> [OnitamaAction]
recorrerTablero game@(OnitamaGame t@(Tablero tablero) cartasR cartasA cartasE jugador) pos funcion
 |pos == 24 = []
 |(tablero!!(fromIntegral pos)) == (Peon jugador) || (tablero!!(fromIntegral pos)) == (Maestro jugador) = funcion game pos ++ recorrerTablero game (pos+1)
 |otherwise = recorrerTablero game (pos+1) funcion

-- crearMov :: Pieza -> Tablero -> Integer -> [OnitamaCard] -> OnitamaPlayer -> [OnitamaAction]
crearMov :: OnitamaGame -> Integer ->  [OnitamaAction]
crearMov _ 24 = []
crearMov game@(OnitamaGame t@(Tablero tablero) cR cB cE j) pos =(movimientosPosibles (posACord pos) (cartaATupla carta) t carta j) ++ crearMov game pos
   where (carta:baraja) = cartasJugador game 


--antes movimientos 
movimientosPosibles :: (Integer,Integer) -> [(Integer,Integer)] -> Tablero -> OnitamaCard -> OnitamaPlayer -> [OnitamaAction] 
movimientosPosibles (x,y) [] _ _ _ = []
movimientosPosibles (x,y) ((a,b):lista) tablero carta jugador 
 |puedeMovAsi (x,y) carta (x-a,y-b) tablero && (jugador == BluePlayer) = [(OnitamaAction (x,y) carta (x-a,y-b))] ++ movimientosPosibles (x,y) lista tablero carta jugador
 |puedeMovAsi (x,y) carta (x+a,y+b) tablero && (jugador == RedPlayer) = [(OnitamaAction (x,y) carta (a+x,b+y))] ++ movimientosPosibles (x,y) lista tablero carta jugador
 |otherwise = movimientosPosibles (x,y) lista tablero carta jugador

 -- Esta función aplica una acción sobre un estado de juego dado, y retorna el estado resultante. Se debe levantar un error si eljugador dado no es el jugador activo, si el juego está terminado, o si la acción no es realizable.
next :: OnitamaGame -> OnitamaPlayer -> OnitamaAction -> OnitamaGame
next game@(OnitamaGame table c cz ce j) jugador accion
 | activePlayer game /= jugador = error "No puede jugar dos veces seguidas"
 | elem accion (recorrerTablero game 0 crearMov) == False = error "No se puede realizar ese movimiento!" 
 | elem accion (recorrerTablero game 0 crearMov) = OnitamaGame (actualizoTablero table accion) c cz ce (otroPlayer jugador)
 | otherwise = error "No has introducido un onitamaGame. Su llamado a esta función es imposible de procesar."

deck = [Tiger , Dragon , Frog , Rabbit , Crab , Elephant , Goose , Rooster , Monkey , Mantis , Horse , Ox , Crane , Boar , Eel , Cobra]


result :: OnitamaGame -> [GameResult OnitamaPlayer]
result game = []  

showGame :: OnitamaGame -> String
showGame g = show g --TODO

showAction :: OnitamaAction -> String
showAction a = show a --TODO
   
readAction :: String -> OnitamaAction
readAction texto = (OnitamaAction (1,1) Tiger (1,0)) --TODO (OnitamaAction (int,int) carta (int,int)) "(1,1) Tiger (3,1)"

players :: [OnitamaPlayer]
players = []

------------------- AUXILIARES -------------------
--cordenada a posicion
cordAPos :: (Integer, Integer) -> Integer
cordAPos (x, y) = x + y * 5
--Posicion a cordenada 
posACord :: Integer -> (Integer, Integer)
posACord pos = (pos - 5 * (div pos 5), div pos 5)

-- Actualiza el tablero dado, con la acción dada. (básicamente conformando la lógica del next):
--Retorna maybe tablero, nothing en caso de que no se pueda realizar la accion, tablero en caso de que si.

--cambiar para actualizar juego, porque tiene que cambiar la mano de los jugadores al ejecutar un movimiento, carta jugada <-> carta especial
actualizoTablero :: Tablero -> OnitamaAction -> Tablero
actualizoTablero t@(Tablero lista) act@(OnitamaAction (x,y) c (xf,yf)) = (Tablero (modificoLista (cordAPos (x,y)) Vacio (Tablero (modificoLista (cordAPos (xf,yf)) (lista!!(fromIntegral(cordAPos (x,y)))) t)))) 

modificoLista :: Integer -> Pieza -> Tablero -> [Pieza]
modificoLista _ _ (Tablero []) = []
modificoLista pos val (Tablero (x:xs))
 | pos == 0 = (val:xs)
 | otherwise = x:(modificoLista (pos-1) val (Tablero xs))

-- otroPlayer, devuelve el jugador contrario al que le toca (utilizado para la lógica del next):
otroPlayer :: OnitamaPlayer -> OnitamaPlayer
otroPlayer (RedPlayer) = BluePlayer
otroPlayer (BluePlayer) = RedPlayer

cartasJugador :: OnitamaGame -> [OnitamaCard]
cartasJugador (OnitamaGame tablero cartasR cartasA cartasE jugador) = if jugador == RedPlayer then cartasR else cartasA 

--revisa si cae en afuera del tablero o 
puedeMovAsi :: (Integer, Integer) -> OnitamaCard -> (Integer, Integer) -> Tablero -> Bool
puedeMovAsi (x,y) c (xf,yf) t@(Tablero lista) = xf >= 0 && xf <5 && yf >= 0 && yf <5 && not (sonDelMismo (piezaAJugador (lista!!(fromIntegral(cordAPos(x,y))))) (piezaAJugador (lista!!( fromIntegral(cordAPos(xf,yf))))))

sonDelMismo :: Maybe OnitamaPlayer -> Maybe OnitamaPlayer -> Bool
sonDelMismo p pz = if (isJust p) && (isJust pz) then 

piezaAJugador :: Pieza -> Maybe OnitamaPlayer
piezaAJugador (Peon j) = Just j
piezaAJugador (Maestro j) = Just j
piezaAJugador (Vacio) = Nothing

-- [(x,y)] x,y son posiciones de la matriz tablero
--TODO revisar
cartaATupla :: OnitamaCard -> [(Integer,Integer)]
cartaATupla (Tiger) = [(0,-1),(0, 2)] 
cartaATupla (Dragon) = [(1,-1),(-1,-1),(2,1),(-2,1)]
cartaATupla (Frog) = [(1,-1),(-2,0),(-1,1)]
cartaATupla (Rabbit) = [(-1,-1),(1,1),(2,0)]
cartaATupla (Crab) = [(-2,0),(0,1),(2,0)]
cartaATupla (Elephant) = [(-1,1),(-1,0),(1,1),(1,0)]
cartaATupla (Goose) = [(-1,1),(-1,0),(1,-1),(1,0)]
cartaATupla (Rooster) = [(-1,-1),(-1,0),(1,1),(1,0)]
cartaATupla (Monkey) = [(-1,-1),(-1,1),(1,-1),(1,1)]
cartaATupla (Mantis) = [(-1,1),(1,1),(0,1)]
cartaATupla (Horse) = [(-1,0),(0,-1),(0,1)]
cartaATupla (Ox) = [(1,0),(0,-1),(0,1)] 
cartaATupla (Crane) = [(-1,-1),(0,1),(1,-1)] 
cartaATupla (Boar) = [(-1,0),(0,1),(1,0)] 
cartaATupla (Eel) = [(-1,-1),(1,0),(-1,1)] 
cartaATupla (Cobra) = [(1,-1),(1,1),(-1,0)] 

tableroInicial = (Tablero ((replicate 2 (Peon RedPlayer)) ++ [(Maestro RedPlayer)] ++ (replicate 2 (Peon RedPlayer)) ++ (replicate 15 Vacio) ++ (replicate 2 (Peon BluePlayer)) ++ [(Maestro BluePlayer)] ++ (replicate 2 (Peon BluePlayer))))

{-
{-- Match controller -------------------------------------------------------------------------------

Código de prueba. Incluye una función para correr las partidas y dos agentes: consola y aleatorio.

-}
type OnitamaAgent = OnitamaGame -> IO (Maybe OnitamaAction)

{- La función ´runMatch´ corre la partida completa a partir del estado de juego dado, usando los dos 
agentes dados. Retorna una tupla con los puntajes (score) finales del juego.
-}
runMatch :: (OnitamaAgent, OnitamaAgent) -> OnitamaGame -> IO [GameResult OnitamaPlayer]
runMatch ags@(ag1, ag2) g = do
   putStrLn (showGame g)
   case (activePlayer g) of
      Nothing -> return $ result g
      Just p -> do
         let ag = [ag1, ag2] !! (fromJust (elemIndex p players))
         move <- ag g
         runMatch ags (Onitama.next g p (fromJust move))

shuffle :: [a] -> IO [a]
shuffle vs = do
  let len = length vs
  rs <- mapM randomRIO (take len (repeat (0.0::Double, 1.0)))
  return [vs !! i | (_, i) <- sort (zip rs [0..(len - 1)])]

runGame :: (OnitamaAgent, OnitamaAgent) -> IO [GameResult OnitamaPlayer]
runGame ags = do
  cards <- shuffle deck
  runMatch ags (beginning cards)

{- El agente de consola ´consoleAgent´ muestra el estado de juego y los movimientos disponibles por
consola, y espera una acción por entrada de texto.
-}
consoleAgent :: OnitamaPlayer -> OnitamaAgent
consoleAgent player state = do
   let moves = fromJust (lookup player (actions state))
   if null moves then do
      putStrLn "No moves!"
      getLine
      return Nothing
   else do
      putStrLn ("Select one move:" ++ concat [" "++ show m | m <- moves])
      line <- getLine
      let input = readAction line
      if elem input moves then return (Just input) else do 
         putStrLn "Invalid move!"
         consoleAgent player state


{- Las funciones ´runConsoleGame´ y `runConsoleMatch` ejecutan toda la partida 
usando dos agentes de consola.
-}
runConsoleGame :: IO [GameResult OnitamaPlayer]
runConsoleGame = do
   runGame (consoleAgent RedPlayer, consoleAgent BluePlayer)
runConsoleMatch :: OnitamaGame -> IO [GameResult OnitamaPlayer]
runConsoleMatch g = do
   runMatch (consoleAgent RedPlayer, consoleAgent BluePlayer) g

{- El agente aleatorio ´randomAgent´ elige una acción de las disponibles completamente al azar.
-}
randomAgent :: OnitamaPlayer -> OnitamaAgent
randomAgent player state = do
    let moves = fromJust (lookup player (actions state))
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else do
       i <- randomRIO (0, (length moves) - 1)
       return (Just (moves !! i))

{- Las funciones ´runRandomGame´ y `runRandomMatch` ejecutan toda la partida 
usando dos agentes aleatorios.
-}
runRandomGame :: IO [GameResult OnitamaPlayer]
runRandomGame = do
   runGame (randomAgent RedPlayer, randomAgent BluePlayer)
runRandomMatch :: OnitamaGame -> IO [GameResult OnitamaPlayer]
runRandomMatch g = do
   runMatch (randomAgent RedPlayer, randomAgent BluePlayer) g

-- Fin
--Obtiene una acción a partir de un texto que puede habersido introducido por el usuario en la consola.
-- readAction :: String -> OnitamaAction
-}

