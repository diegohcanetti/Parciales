module Lib where
import Text.Show.Functions

--- Punto 01 ---
--- se tiene la posibilidad de chasquear un universo que contiene a todos sus habitantes y reducir a la mitad la cantidad de dichos personajes. 
--- Por ejemplo si tenemos un universo en el cual existen ironMan, drStrange, groot y wolverine, solo quedan los dos primeros que son ironMan y drStrange. 
--- Si además de los 4 personajes estuviera viudaNegra, quedarían también ironMan y drStrange porque se considera la división entera.

data Guantelete = Guantelete {
    material :: String,
    gemas    :: [Gema]
} deriving (Show)

data Personaje = Personaje {
    edad :: Int,
    energia :: Float,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
} deriving (Eq, Show)

type Gema = Personaje -> Personaje
type Universo = [Personaje]
type FuncionUniverso = Universo -> Universo

chasquearUniverso :: Guantelete -> Universo -> Universo
chasquearUniverso guantelete universo 
    | guanteleteCompleto guantelete = reducirMitad universo
    | otherwise                     = universo

guanteleteCompleto :: Guantelete -> Bool
guanteleteCompleto guantelete = tieneSuficientesGemas guantelete && (material guantelete) == "uru"

tieneSuficientesGemas :: Guantelete -> Bool
tieneSuficientesGemas = (==6) . length . gemas 

reducirMitad :: FuncionUniverso
reducirMitad universo = take (length universo `div` 2)  universo

--- Punto 02 ---
--- a. Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años ---

pendex :: Universo -> Bool
pendex = any ((<45) . edad)

--- b.Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad. ---

energiaTotal :: Universo -> Float
energiaTotal = sum . map energia . filter masDeUnaHabilidad 

masDeUnaHabilidad :: Personaje -> Bool
masDeUnaHabilidad =  (>1) . length . habilidades

--- Punto 03 ---

--- La mente que tiene la habilidad de debilitar la energía de un usuario en un valor dado. ---

mente :: Float -> Gema
mente  = quitarEnergia  

--- eliminar una habilidad en particular si es que la posee. Además le quita 10 puntos de energía. ---

alma :: String -> Gema
alma habilidad  = quitarEnergia 10  . modificarHabilidades (filter( /=habilidad) )

quitarEnergia :: Float -> Personaje -> Personaje
quitarEnergia numero personaje = personaje {energia = energia personaje - numero}

---  transportar al rival al planeta x (el que usted decida) y resta 20 puntos de energía. ---

espacio :: String -> Gema
espacio planeta = quitarEnergia 20 . transportarAlPlaneta planeta

transportarAlPlaneta :: String -> Personaje -> Personaje
transportarAlPlaneta nuevoPlaneta personaje = personaje {planeta = nuevoPlaneta}

--- El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita (en caso contrario no le saca ninguna habilidad).

poder :: Gema
poder  personaje = quitarEnergia (energia personaje) . quitarHabilidad  $ personaje
    where
        quitarHabilidad :: Personaje-> Personaje
        quitarHabilidad habilidades
            | menosDeDosHabilidades  habilidades = modificarHabilidades (const []) personaje
            | otherwise                          = habilidades


modificarHabilidades :: ([String] -> [String]) -> Personaje -> Personaje
modificarHabilidades funcion personaje = personaje {habilidades = funcion (habilidades personaje)}

menosDeDosHabilidades :: Personaje -> Bool
menosDeDosHabilidades =  (<=2) . length . habilidades

ironMan :: Personaje
ironMan = Personaje 30 40 ["Super Fuerza"] "Iron Man" "Tierra"

tiempo :: Gema
tiempo personaje
    | quedaEn18 personaje = reducirEdad . quitarEnergia 50 $ personaje
    | otherwise = quitarEnergia 50 personaje {edad = 18} 

--- Otra tiempo personaje = quitarEnergia 50 personaje {edad = (max 18.div (edad personaje)) 2 }

reducirEdad :: Personaje -> Personaje
reducirEdad personaje =  personaje {edad = (edad personaje) `div` 2}

quedaEn18 :: Personaje -> Bool
quedaEn18 personaje = ((edad personaje) `div` 2) >= 18 

gemaLoca :: Gema -> Gema
gemaLoca gema = gema . gema

--- Punto 04 ---

guanteleteGoma :: Guantelete
guanteleteGoma = Guantelete "Goma" [tiempo, alma  "usar Mjolnir", gemaLoca (alma "programación en Haskell") ]

--- Punto 05 ---
--- Generar la función utilizar  que dado una lista de gemas y un enemigo ejecuta el poder de cada una de las gemas que lo componen contra el personaje dado. 
--- Indicar cómo se produce el “efecto de lado” sobre la víctima.

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas enemigo = foldr ($) enemigo $ gemas

--- Punto 06 ---
--- Definir la función gemaMasPoderosa que dado un guantelete y una persona obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima. 

gemaMasPoderosa :: Personaje -> Guantelete -> Gema
gemaMasPoderosa personaje  = gemaMasPoderosaDe personaje . gemas 

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe _ [gema] = gema
gemaMasPoderosaDe personaje (gema1:gema2:gemas) 
    | (energia.gema1) personaje < (energia.gema2) personaje = gemaMasPoderosaDe personaje (gema1:gemas)
    | otherwise = gemaMasPoderosaDe personaje (gema2:gemas)
