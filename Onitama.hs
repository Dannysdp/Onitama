{- Onitama ------------------------------------------------------------------------------------------
 
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
import Data.Char
import Data.Maybe

-- Data representativa de las acciones del juego, construída con: (pos_inicial_x , pos_inicial_y, pos_final_x, pos_final_y)
data OnitamaAction = OnitamaAction (Integer, Integer) OnitamaCard (Integer, Integer) deriving(Eq, Show)

--Data representativa de las cartas del juego (constructores vacíos, las tuplas que representan sus movimientos son funciones auxiliares deterministas).
data OnitamaCard = Tiger | Dragon | Frog | Rabbit | Crab | Elephant | Goose | Rooster | Monkey | Mantis | Horse | Ox | Crane | Boar | Eel | Cobra deriving(Show, Eq)

--Data representativa de un jugador del juego (rojo o azul).
data OnitamaPlayer = RedPlayer | BluePlayer deriving(Eq, Show, Enum)

-- Data representativa de el juego en sí, construída con: Tablero [cartas_azul] [cartas_rojo] [carta_extra] jugador_actual.
data OnitamaGame = OnitamaGame Tablero [OnitamaCard] [OnitamaCard] [OnitamaCard] OnitamaPlayer   -- tablero actual, cartasR, cartasB, carta extra y juegador

--Data representativa del resultado del juego (ganador y perdedor, donde p es un jugador).
data GameResult p = Winner p | Loser p deriving(Eq, Show)

-- Data representativa de las piezas, donde peon y maestro reciben un jugador (su dueño), mientras que vacio no (evidentemente).
data Pieza = Peon OnitamaPlayer | Maestro OnitamaPlayer | Vacio deriving (Eq,Show)

--Data representativa del tablero del juego.
data Tablero = Tablero [Pieza] deriving(Show)

--Data para variantes EXTRA
data OnitamaConfig = OnitamaConfig { configDeck :: [OnitamaCard], configHandSize :: Int, configStalemate :: Bool} deriving (Eq, Show)

{-
Dibujo representativo del tablero, a modo de ayuda para el programador.
                  cordenadas x y           pos lista 
 r1 r2 rr r3 r4 | 0,0 1,0 2,0 3,0 4,0  -> 0  1  2  3  4 
  -  -  -  -  - | 0,1 1,1 2,1 3,1 4,1  -> 5  6  7  8  9 
  -  -  -  -  - | 0,2 1,2 2,2 3,2 4,2  -> 10 11 12 13 14 
  -  -  -  -  - | 0,3 1,3 2,3 3,3 4,3  -> 15 16 17 18 19 
 a1 a2 ra a3 a4 | 0,4 1,4 2,4 3,4 4,4  -> 20 21 22 23 24 
 x = horizontal y = veritcal 
 convertir de pos lista a cordenadas = x + y*5
-}


{- Enumerado con todas las cartas del juego. -}
deck :: [OnitamaCard] 
deck = [Tiger , Dragon , Frog , Rabbit , Crab , Elephant , Goose , Rooster , Monkey , Mantis , Horse , Ox , Crane , Boar , Eel , Cobra]

{- Recibe una lista de cartas, y devuelve una bajara con las 2 primeras para el jugador rojo, el par siguiente para el azul
y la quinta como extra. El shuffle se realiza solo, en el llamado de la función hecha por el código plantilla de los profesores. -}
beginning :: [OnitamaCard] -> OnitamaGame -- yo doy barajas
beginning baraja = (OnitamaGame tableroInicial (fst cartas1) (fst cartas2) (fst cartaE) RedPlayer )
    where 
        cartas1 = (splitAt 2 baraja)
        cartas2 = (splitAt 2 (snd cartas1))
        cartaE = (splitAt 1 (snd cartas2))

{- Devuelve el jugador al que le toca -}
activePlayer :: OnitamaGame -> Maybe OnitamaPlayer
activePlayer game@(OnitamaGame  _ _ _ _ jugador ) = if result game == [] then Just jugador else Nothing

{- Devuelve una lista con todas las acciones del jugador que le toca. Utiliza métodos auxiliares los cuales se explican en su
correspondiente ocurrencia en el código. -}
actions :: OnitamaGame -> [(OnitamaPlayer, [OnitamaAction])]
actions game@(OnitamaGame tablero cartasR cartasA cartasE jugador ) = [(RedPlayer, if jugador == RedPlayer then actionsRed else []), (BluePlayer, if jugador == BluePlayer then actionsBlue else [])]
    where actionsRed = recorrerTablero game 0 
          actionsBlue = recorrerTablero game 0 

{- Evalúa un estado de juego, un jugador y el movimiento que quiere realizar, devolviendo un nuevo estado de juego con el movimiento realizado. -}
next :: OnitamaGame -> OnitamaPlayer -> OnitamaAction -> OnitamaGame
next game@(OnitamaGame table c cz ce j ) jugador accion@(OnitamaAction _ cu _)
 | activePlayer game == Nothing = error "El juego ha terminado!"
 | activePlayer game /= Just jugador = error "No puede jugar dos veces seguidas"
 | elem accion (recorrerTablero game 0) == False = error "No se puede realizar ese movimiento!" 
 | (jugador == BluePlayer && elem accion (recorrerTablero game 0)) = OnitamaGame (actualizoTablero table accion) c (fst cblue) (snd cblue) (otroPlayer jugador) 
 | (jugador == RedPlayer && elem accion (recorrerTablero game 0)) = OnitamaGame (actualizoTablero table accion) (fst cred) cz (snd cred) (otroPlayer jugador) 
 | otherwise = error "No has introducido un onitamaGame. Su llamado a esta función es imposible de procesar."
   where cred = cambioCartas cu c ce
         cblue = cambioCartas cu cz ce


{- Evalúa cada opción posible de derrota en base a las posiciones del tablero. "cond" es la opción para verificar ahogamiento del jugador actual -}
result :: OnitamaGame -> [GameResult OnitamaPlayer]
result game@(OnitamaGame t@(Tablero tablero) _ _ _ p )
 |tablero!!22 == Maestro RedPlayer = [Winner RedPlayer, Loser BluePlayer]
 |tablero!!2 == Maestro BluePlayer = [Winner BluePlayer, Loser RedPlayer]
 |not $ elem (Maestro RedPlayer) tablero = [Winner BluePlayer, Loser RedPlayer]
 |not $ elem (Maestro BluePlayer) tablero = [Winner RedPlayer, Loser BluePlayer]
 |otherwise = [] 

{- Muestra la configuracion del juego por pantalla, incluyendo el tablero y la informacion util. -}
showGame :: OnitamaGame -> String
showGame (OnitamaGame (Tablero lista) cartasO cartasT extra jugador) = ("\n" ++ "Los jugadores disponen de " ++ show (length cartasT) ++ " cartas cada uno.\n" ++ "Ahogamiento como derrota seteado en: " ++ "\n\n" ++ "|" ++ showTablero lista 1 ++ "\nEl jugador ROJO tiene las cartas: " ++ show cartasO ++"\nEl jugador AZUL tiene las cartas: " ++ show cartasT ++ "\nLa carta extra es: "++ show extra ++"\nEl siguiente jugador en mover es: " ++ show jugador ++ "\n")

{- Muestra una accion por pantalla -}
showAction :: OnitamaAction -> String
showAction (OnitamaAction (a,b) card (c,d)) = ("Mueve desde la posicon (" ++ show a ++ "," ++ show b ++ "), con la carta " ++ show card ++ ", hacia (" ++show c ++","++ show d ++ ")")

{- Lee un string recibido, y lo transforma en acción, separando por espacios el user-input. 
IMPORTANTE:
Si el usuario introduce una String de un movimiento imposible, el estado generado al aplicar esa acción lanzará un error, y se cortará la ejecución.
De la misma manera, si el jugador introduce una String con letras de más, o innecesarias, ésta (función) no se ejecutará ) -}
readAction :: String -> OnitamaAction
readAction s = let x = splitStr s ' ' in OnitamaAction (stringToTuple (x !! 0)) (stringToCard (x !! 1)) (stringToTuple (x !! 2))

------------------- AUXILIARES -------------------
--cordenada a posicion
cordAPos :: (Integer, Integer) -> Integer
cordAPos (x, y) = x + y * 5
--Posicion a cordenada 
posACord :: Integer -> (Integer, Integer)
posACord pos = (pos - 5 * (div pos 5), div pos 5)

{-
-- Actualiza el tablero dado, con la acción dada. (básicamente conformando la lógica del next):
--Retorna maybe tablero, nothing en caso de que no se pueda realizar la accion, tablero en caso de que si.
-- retorna una lista de llamados a resultados dada una lista de acciones y un juego. En otras palabras, aplica todas las acciones posibles
-- al juego actual, y la devuelve la lista de resultados hechos a los juegos luego de haber sido aplicada(con next) cada una de esas acciones.
-}

resultActList :: OnitamaGame -> OnitamaPlayer -> [OnitamaAction] -> [([GameResult OnitamaPlayer],OnitamaAction)]
resultActList g p [] = []
resultActList g p (act:actList) = [(result (Onitama.next g p act),act)] ++ resultActList g p actList


{-
Actualiza el tablero, realizando el movimiento que le es mandado.
-}
actualizoTablero :: Tablero -> OnitamaAction -> Tablero
actualizoTablero t@(Tablero lista) act@(OnitamaAction (x,y) c (xf,yf)) = (Tablero (modificoLista (cordAPos (x,y)) Vacio (Tablero (modificoLista (cordAPos (xf,yf)) (lista!!(fromIntegral(cordAPos (x,y)))) t)))) 

{-
Modifica una lista, dada una posición, un valor, y la lista. (podría ser general para tipo a, sin embargo, no es necesario trabajar con Generics)
Evidentemente, devuelve la lista con el valor reemplazado en la posición dada (eliminando el valor original de esa posición)
-}
modificoLista :: Integer -> Pieza -> Tablero -> [Pieza]
modificoLista _ _ (Tablero []) = []
modificoLista pos val (Tablero (x:xs))
 | pos == 0 = (val:xs)
 | otherwise = x:(modificoLista (pos-1) val (Tablero xs))


{-
Devuelve al otro jugador, dado un jugador.
-}
otroPlayer :: OnitamaPlayer -> OnitamaPlayer
otroPlayer (RedPlayer) = BluePlayer
otroPlayer (BluePlayer) = RedPlayer

{-
Revisa si el movimiento cae afuera del tablero o sobre una pieza propia.
-}
puedeMovAsi :: (Integer, Integer) -> OnitamaCard -> (Integer, Integer) -> Tablero -> Bool
puedeMovAsi (x,y) c (xf,yf) t@(Tablero lista) = xf >= 0 && xf <5 && yf >= 0 && yf <5 && not (sonDelMismo (piezaAJugador (lista!!(fromIntegral(cordAPos(x,y))))) (piezaAJugador (lista!!( fromIntegral(cordAPos(xf,yf))))))

{-
Verifica si 2 jugadores son iguales. 
Aunque parezca innecesaria, debido a la estructura de que vacío no tiene jugador, es necesario trabajar con maybes, lo cual
hace que esta función sea completamente necesaria ante la incapacidad de evaluar el jugador de un espacio vacío.
-}
sonDelMismo :: Maybe OnitamaPlayer -> Maybe OnitamaPlayer -> Bool
sonDelMismo p pz 
 | (isJust p && isJust pz) = p == pz
 | (isJust p && not (isJust pz)) = False 
 | (not (isJust p) && isJust pz) =  False
 | otherwise = True

{-
Recibe una pieza, y devuelve el jugador el cual es dueño de ella. Si la pieza es vacío, devuelve nothing.
-}
piezaAJugador :: Pieza -> Maybe OnitamaPlayer
piezaAJugador (Peon j) = Just j
piezaAJugador (Maestro j) = Just j
piezaAJugador (Vacio) = Nothing

--Auxiliar de actions 
-- llama las funsion crearMov para las piezas en el tablero del jugador actual 
recorrerTablero :: OnitamaGame -> Integer -> [OnitamaAction]
recorrerTablero game@(OnitamaGame t@(Tablero tablero) cartasR cartasA cartasE jugador ) pos 
 |pos == 24 = []
 |(tablero!!(fromIntegral pos)) == (Peon jugador) || (tablero!!(fromIntegral pos)) == (Maestro jugador) = crearMov game pos ++ recorrerTablero game (pos+1) 
 |otherwise = recorrerTablero game (pos+1) 
--Auxiliar de actions 
-- revisa que mano de cartas usar y las recorre llamando movimientosPosibles
crearMov :: OnitamaGame -> Integer ->  [OnitamaAction]
crearMov game@(OnitamaGame t@(Tablero tablero) [] [] cE j ) _ = []
crearMov game@(OnitamaGame t@(Tablero tablero) (cartaR:cR) (cartaB:cB) cE j ) pos
 |j == RedPlayer = (movimientosPosibles (posACord pos) (cartaATupla cartaR) t cartaR j) ++ crearMov (OnitamaGame t cR cB cE j ) pos
 |j == BluePlayer = (movimientosPosibles (posACord pos) (cartaATupla cartaB) t cartaB j) ++ crearMov (OnitamaGame t cR cB cE j ) pos
--Auxiliar de actions 
--Recorre las tuplas dentro de cada carta, creando los movimientos para la pieza.
movimientosPosibles :: (Integer,Integer) -> [(Integer,Integer)] -> Tablero -> OnitamaCard -> OnitamaPlayer -> [OnitamaAction] 
movimientosPosibles (x,y) [] _ _ _ = []
movimientosPosibles (x,y) ((a,b):lista) tablero carta jugador 
 |puedeMovAsi (x,y) carta (x+a,y-b) tablero && (jugador == BluePlayer) = [(OnitamaAction (x,y) carta (x+a,y-b))] ++ movimientosPosibles (x,y) lista tablero carta jugador
 |puedeMovAsi (x,y) carta (x+a,y+b) tablero && (jugador == RedPlayer) = [(OnitamaAction (x,y) carta (a+x,b+y))] ++ movimientosPosibles (x,y) lista tablero carta jugador
 |otherwise = movimientosPosibles (x,y) lista tablero carta jugador


{-
recibe (en orden) carta usada, mano actual, carta extra, devuelve (manoNueva, cartaExtra)
-}
cambioCartas :: OnitamaCard -> [OnitamaCard] -> [OnitamaCard] -> ([OnitamaCard],[OnitamaCard])
cambioCartas cu cj ce = ((ce++[x | x <- cj, x /= cu]), [cu]) 

{-
Elemento encargado de mostrar el tablero. Se podría hacer mejor con un instance show, más estructurado sería.
-}
showTablero :: [Pieza] -> Integer -> String
showTablero (pi:prestantes) pos 
 | prestantes == [] = piezaAString pi ++ "\n"
 | ((mod pos 5) == 0) = piezaAString pi ++ "\n" ++ "|" ++showTablero prestantes (pos+1)
 | otherwise = piezaAString pi ++ showTablero prestantes (pos+1)

{-
Pasa una Pieza a formato string, para ser usado en showTablero.
PR es peón rojo(red), PB es peón azul(blue), MR es maestro rojo, MB es maestro azul, y o es vacío.
-}
piezaAString :: Pieza -> String 
piezaAString p
 | p==(Peon RedPlayer) = " PR|"
 | p==(Peon BluePlayer) = " PB|"
 | p==(Maestro RedPlayer) = " MR|"
 | p==(Maestro BluePlayer) = " MB|"
 | otherwise = " o |"

{-
Toma una string con la estrutcura (x,y), donde x e y son números, y devuelve una tupla de la forma (x,y).
Utilizada para readAction.
Claramente, si la string enviada no cumple esa estructura, el llamado retornará cualquier cosa, o no funcionarán correctamente los read.
-}
stringToTuple :: String -> (Integer,Integer)
stringToTuple s = ( read [(s !! 1)]::Integer , read [(s !! 3)]::Integer)

{-
Toma una string con el nombre de una carta, sea todo en minúsculas o con primera letra en mayúscula, y devuelve la carta correspondiente.
Utilizada para readAction.
Claramente, si la string enviada no cumple ninguna de las condiciones, se devolverá un error.
-}
stringToCard :: String -> OnitamaCard
stringToCard s 
 | s == "Mantis" || s == "mantis" = Mantis
 | s == "Frog" || s == "frog" = Frog
 | s == "Ox" || s == "ox" = Ox   
 | s == "Rooster" || s == "rooster" = Rooster
 | s == "Boar" || s == "boar" = Boar
 | s == "Crane" || s == "crane" = Crane
 | s == "Monkey" || s == "monkey" = Monkey
 | s == "Horse" || s == "horse" = Horse
 | s == "Cobra" || s == "cobra" = Cobra
 | s == "Goose" || s == "goose" = Goose
 | s == "Elephant" || s == "elephant" = Elephant
 | s == "Tiger" || s == "tiger" = Tiger
 | s == "Rabbit" || s == "rabbit" = Rabbit
 | s == "Crab" || s == "crab" = Crab
 | s == "Dragon" || s == "dragon" = Dragon
 | s == "Eel" || s == "eel" = Eel
 | otherwise = error ("No has ingresado una carta! Juego cancelado.")

{-
Ideado en base a la función .split(), de Java.
Su funcionamiento es similar, aunque su "coding" totalmente distinto.
Recibe una string, un character el cual se usará para separar, y devuelve una lista con las strings separadas por ese character.
-}
splitStr :: String -> Char -> [String]
splitStr [] _ = []
splitStr s c = if head s == c then splitStr (tail s) c else [n] ++ splitStr (drop (length n) s) c 
    where n = takeWhile (/=c) s

{-
Recibe una carta, y la convierte en una tupla que contiene todas las "variaciones de movimientos" que realiza esa carta.
NO representa todos los movimientos posibles, sino tendría miles de renglones. Representa las variaciones de posición (estándar) en el tablero.
-}
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

{-
Como lo dice su nombre, es el tablero inicial. Es una constante, ya que siempre es el mismo (valga la redundancia).
-}

tableroInicial :: Tablero
tableroInicial = (Tablero ((replicate 2 (Peon RedPlayer)) ++ [(Maestro RedPlayer)] ++ (replicate 2 (Peon RedPlayer)) ++ (replicate 15 Vacio) ++ (replicate 2 (Peon BluePlayer)) ++ [(Maestro BluePlayer)] ++ (replicate 2 (Peon BluePlayer))))

{- Función que devuelve una lista con los 2 jugadores posibles. Bastante simple.
-}
players :: [OnitamaPlayer]
players = [BluePlayer,RedPlayer]

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