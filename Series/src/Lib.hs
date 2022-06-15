module Lib where

import Text.Show.Functions
import Data.Char

data Serie = Serie {
    nombreSerie :: String,
    actores :: [Actor],
    presupuesto :: Int,
    temporadasEstimadas :: Int,
    ratingPromedio :: Int,
    cancelada :: Bool
} deriving (Show)

data Actor = Actor {
    nombreActor :: String,
    sueldo :: Int,
    restricciones :: [String]
} deriving (Show)

friends :: Serie
friends = Serie "Friends" [pepe,carolina] 50 8 7 False

pepe :: Actor
pepe = Actor "pepe" 10 []

carolina :: Actor
carolina = Actor "carolina" 20 []

--- Punto 01 --
--a.
serieRojo :: Serie -> Bool
serieRojo serie = (presupuesto serie) < actoresCobran serie

actoresCobran :: Serie -> Int
actoresCobran = sum . map sueldo . actores

--b.
esProblematica :: Serie -> Bool
esProblematica = (>3) . (tieneRestricciones 1)

tieneRestricciones :: Int -> Serie -> Int
tieneRestricciones num  serie = length $ filter (masDeXRestricciones num ) (actores serie)

masDeXRestricciones num = (>num) . length . restricciones 

--- Punto 02 ---
type Productor = Serie -> Serie

cambiarActoresSerie :: ([Actor] -> [Actor]) -> Serie -> Serie
cambiarActoresSerie funcion serie = serie { actores = funcion (actores serie) }

conFavoritismo :: [Actor] -> Productor
conFavoritismo actoresFavoritos = cambiarActoresSerie (++ actoresFavoritos). cambiarActoresSerie (drop 2) 

timBurton :: Productor
timBurton = conFavoritismo [johnnyDepp , helenaBonhamCarter]

johnnyDepp :: Actor
johnnyDepp = Actor "johnny depp" 20000000 []

helenaBonhamCarter :: Actor
helenaBonhamCarter = Actor "helema bonham carter" 15000000 []


gatopardeitor :: Productor
gatopardeitor = id

estireitor :: Productor
estireitor serie = serie {temporadasEstimadas = (temporadasEstimadas serie) * 2}

canceleitor :: Int -> Productor
canceleitor cifra serie = serie {cancelada = ((serieRojo serie) || (ratingBaja cifra serie))} 

ratingBaja :: Int -> Serie -> Bool
ratingBaja cifra serie = (ratingPromedio serie) < cifra

-- Punto 3

bienestarLongitud :: Serie -> Int
bienestarLongitud serie
  | (temporadas serie) > 4 = 5
  | otherwise = (10 - (temporadas serie)) *2

bienestarReparto :: Serie -> Int
bienestarReparto serie
  | (length $ actores serie) < 10 = 3
  | otherwise = 10 - (tieneRestricciones 2 serie) 

bienestar :: Serie -> Int
bienestar serie
  | (cancelada serie) = 0
  | otherwise = (bienestarLongitud serie) + (bienestarReparto serie)

-- Punto 4

productorMasEfectivo :: [Serie] -> [Produccion] -> [Serie]
productorMasEfectivo series productores = map (masEfectivo productores) series

masEfectivo :: [Produccion] -> Serie -> Serie
masEfectivo (x:[]) serie = x serie 
masEfectivo (x:xs) serie
  | bienestar (x serie) > bienestar (head xs $ serie) = x serie
  | otherwise = masEfectivo xs serie

-- Punto 5

-- ¿Se puede aplicar el productor gatopardeitor cuando tenemos una lista infinita de actores?
-- si, se puede aplicar gatopardeitor con una lista infinita de actores. no se traba en consola.
-- como la funcion es la funcion id (identidad) devuelve infinitamente la serie que le paso, con la lista infinita de actores.
-- wl problema es que como tiene que mostrar una lista infinita de actores, nunca llego a ver los demas
-- atributos de la serie (temporadas, rating, etc).
-- si bien funciona en consola, no cumple con el proposito de la funcion.

serieEjemplo :: Serie
serieEjemplo = Serie "serie ejemplo" actoresInfinitos 100 2 5 False

actoresInfinitos = johnnyDepp : actoresInfinitos

-- > Resultados : 
-- Serie {nombre = "serie ejemplo", actores = [Actor {nombreActor = "johnny depp", sueldo = 20000000, restricciones = []},Actor {nombreActor = "johnny depp", sueldo = 20000000, restricciones = []} ....

-- ¿Y a uno con favoritismos? ¿De qué depende?

-- al aplicar conFavoritismo no hay problema al hacer el drop de los primero 2 elementos.
-- cuando se quiere agregar los favoritos a la lista puede ocurrir el problema: si se agregan al principio de la lista
-- no hay problema alguno, pero sí lo hay si se agregan al final de la lista (ya que nunca encontrara el final
-- de una lista infinita). 
-- por lo que depende de si agregamos a los actores al principio o al final

-- Punto 6

esControvertida :: Serie -> Bool
esControvertida serie = not $ cobraMasQueElSiguiente (actores serie)

cobraMasQueElSiguiente :: [Actor] -> Bool
cobraMasQueElSiguiente (x:[]) = True
cobraMasQueElSiguiente (x:xs) = (sueldo x) > (sueldo $ head xs) 

-- Punto 7

-- funcionLoca x y = filter (even.x) . map (length.y)

-- primero sabemos que hay dos parametro : x e y
-- como la primer funcion que se va a aplicar es map, sabemos que hay un tercer parametro implicito: z
-- z es una lista, no sabemos de que
-- funcionLoca :: -> -> [a] -> 
-- como y recibe la lista de z, debe tener su mismo tipo, pero puede devolver algo de otro tipo. lo unico que 
-- sabemos de este algo es que debe ser una lista, pues luego se le aplica la funcion length
-- funcionLoca :: -> (a -> [b]) -> [a] -> 
-- luego, se aplica filter. sabemos que el map devuelve una lista de Int y que sobre esa lista se aplicara el filter.
-- por lo que x es una funcion que recibe Int y devuelve un Int (ya que luego se le aplica even)
-- finalmente la funcion funcionLoca devuelve una lista de Int:
-- funcionLoca :: (Int -> Int) -> (a -> [b]) -> [a] -> [Int]