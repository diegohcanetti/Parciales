module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Jugador = Jugador {
    nombreJugador :: String,
    nombrePadre :: String,
    habilidad :: Habilidad
} deriving (Eq,Show)

data Habilidad = Habilidad {
    fuerza :: Int,
    precisionHabilidad :: Int
} deriving (Eq,Show)

data Tiro = UnTiro {
  velocidad :: Int,
  precisionTiro :: Int,
  altura :: Int
} deriving (Eq, Show)

between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b


type Palo = Habilidad -> Tiro 

putter :: Palo
putter habilidad = UnTiro 10 (doblePrecision habilidad) 0

doblePrecision :: Habilidad -> Int
doblePrecision habilidad = precisionHabilidad habilidad * 2

madera :: Palo
madera habilidad = UnTiro 100 (divisionPrecision habilidad 2) 0

divisionPrecision habilidad numero = precisionHabilidad habilidad `div` numero

hierro :: Int -> Palo
hierro numero habilidad = UnTiro (fuerza habilidad * numero) (divisionPrecision habilidad numero) (max (numero-3) 0)

-- b. Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego --

palos :: [Palo]
palos = [putter , madera] ++ map hierro [1..10]

--- Punto 02 ----
--- Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con las habilidades de la persona.

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

golpe' :: Palo -> Jugador -> Tiro
golpe' palo = palo.habilidad

--- Punto 03 ---
--- Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, independientemente de la velocidad del tiro. 
--- Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0.

type Obstaculo = Tiro -> Tiro
type CondicionTiro = Tiro -> Bool

--Para no repetir la lógica
obstaculoSuperableSi :: CondicionTiro -> Obstaculo -> Obstaculo
obstaculoSuperableSi condicion efecto tiroOriginal
  | condicion tiroOriginal = efecto tiroOriginal
  | otherwise = tiroDetenido

superaTunelConRampita :: CondicionTiro
superaTunelConRampita tiro = precisionTiro tiro > 90 && vaAlrasdelSuelo tiro

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita unTiro = UnTiro {velocidad = velocidad unTiro * 2, precisionTiro = 100, altura = 0}

tunelConRampita :: Obstaculo
tunelConRampita = obstaculoSuperableSi superaTunelConRampita efectoTunelConRampita

vaAlrasdelSuelo :: CondicionTiro
vaAlrasdelSuelo = (==0).altura

tiroDetenido :: Tiro
tiroDetenido = UnTiro 0 0 0

---Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. 
---Luego de superar una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original dividida por el largo de la laguna.

laguna :: Int -> Obstaculo
laguna largo = obstaculoSuperableSi superaLaguna (efectoLaguna largo)

superaLaguna :: CondicionTiro
superaLaguna tiro = (velocidad tiro) > 80 && between 1 5 (altura tiro) 

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo tiroOriginal = tiroOriginal {altura = altura tiroOriginal `div` largo}

---Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. 
---Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.

hoyo :: Obstaculo
hoyo = obstaculoSuperableSi superaHoyo efectoHoyo
    where
    superaHoyo :: CondicionTiro
    superaHoyo unTiro = between 5 20 (velocidad unTiro) && vaAlrasdelSuelo unTiro && (precisionTiro unTiro) > 95
    efectoHoyo _ = tiroDetenido

--- Punto 04 ---

--- a. Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo. ---

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (sirveParaSuperar jugador obstaculo) palos

sirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
sirveParaSuperar jugador obstaculo palo = obstaculo (golpe jugador palo)

--- b.Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar. ---

cuantosObstaculosConsecutivosSupera :: Tiro -> [Obstaculo] -> Int
cuantosObstaculosConsecutivosSupera tiro [] = 0
cuantosObstaculosConsecutivosSupera tiro (obstaculo : obstaculos)
  | puedeSuperar obstaculo tiro
      = 1 + cuantosObstaculosConsecutivosSupera (efectoLuegoDeSuperar obstaculo tiro) obstaculos
  | otherwise = 0

--- c.Definir paloMasUtil que recibe una persona y una lista de obstáculos y determina cuál es el palo que le permite superar más obstáculos con un solo tiro. ---

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = maximoSegun (flip cuantosObstaculosConsecutivosSupera obstaculos. golpe jugador)  palos

--- Punto 05 ---
--- Dada una lista de tipo [(Jugador, Puntos)] que tiene la información de cuántos puntos ganó cada niño al finalizar el torneo, 
--- se pide retornar la lista de padres que pierden la apuesta por ser el “padre del niño que no ganó”. Se dice que un niño ganó el torneo si tiene más puntos que los otros niños.

jugadorTorneo = fst
puntosGanados = snd

pierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdenLaApuesta puntosDeTorneo = map (padre.jugadorTorneo).filter ( not.gano ) $ puntosDeTorneo

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDeTorneo puntosDeUnJugador
  = (all ((< puntosGanados puntosDeUnJugador).puntosGanados)
      . filter (/= puntosDeUnJugador)) puntosDeTorneo 