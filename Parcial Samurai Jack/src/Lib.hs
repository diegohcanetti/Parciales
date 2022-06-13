module Lib where
import Text.Show.Functions

data Elemento = UnElemento { 
    tipo :: String, 
    ataque :: Efecto,
    defensa :: Efecto
} deriving (Show,Eq)

data Personaje = UnPersonaje { 
    nombre :: String,
	salud :: Float,
	elementos :: [Elemento],
	anioPresente :: Int 
} deriving (Show,Eq)

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