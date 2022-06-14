module Lib where
import Text.Show.Functions

data Elemento = UnElemento { 
tipo :: String, 
ataque :: Efecto,
defensa :: Efecto
} deriving (Show)

data Personaje = UnPersonaje { 
nombre :: String,
salud :: Float,
elementos :: [Elemento],
anioPresente :: Int 
} deriving (Show)

  
-- modificarAno :: Int -> I

type Efecto = Personaje -> Personaje
--- Punto 01---

modifcarSalud :: (Float -> Float) -> Personaje -> Personaje
modifcarSalud funcion personaje = personaje {salud = funcion . salud  $ personaje}

mandarAlAnio :: Int -> Efecto
mandarAlAnio ano personaje = personaje {anioPresente = ano} 

meditar :: Efecto
meditar  = modifcarSalud (*1.5)

causarDanio :: Float -> Efecto
causarDanio danio = modifcarSalud (max 0 . flip (-) danio)

--- Punto 02 ---

esMalvado :: Personaje -> Bool
esMalvado = (any (esDeTipo "Maldad") . elementos) 

esDeTipo :: String -> Elemento -> Bool
esDeTipo unTipo elemento = tipo elemento == unTipo

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = salud personaje - salud (ataque elemento personaje)

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje  = filter (esEnemigoMortal personaje) 

esEnemigoMortal personaje  = (any (tieneAtaqueMortal personaje) . elementos) 

tieneAtaqueMortal personaje elemento = danioQueProduce personaje elemento == salud personaje

--- Punto 03 --- 
concentracion :: Int-> Elemento -> Elemento
concentracion nivelConcentracion  = modifcarDefensa ((!! nivelConcentracion) . iterate meditar) . modifcarTipo (const "Magia")

modifcarDefensa :: (Efecto -> Efecto) -> Elemento -> Elemento
modifcarDefensa funcion elemento = elemento {defensa = funcion . defensa  $ elemento}

modifcarTipo :: (String -> String) -> Elemento -> Elemento
modifcarTipo funcion elemento = elemento {tipo = funcion . tipo  $ elemento}

--- Punto 04 ---
luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
 |estaMuerto atacante = (defensor, atacante)
 |otherwise = luchar proximoAtacante proximoDefensor
 where proximoAtacante = usarElementos ataque defensor (elementos atacante)
       proximoDefensor = usarElementos defensa atacante (elementos atacante)

usarElementos :: (Elemento -> Personaje -> Personaje) -> Personaje -> [Elemento] -> Personaje
usarElementos funcion personaje elementos = foldl (flip ($)) personaje (map funcion elementos)

estaMuerto  = ((==0).salud)