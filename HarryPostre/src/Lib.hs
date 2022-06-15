module Lib where

import Text.Show.Functions
import Data.Char

--- Punto 01 --
--a.

data Postre = Postre {
    sabores :: [String],
    peso :: Float,
    temperatura :: Int
} deriving (Show, Eq)

bizcocho :: Postre
bizcocho = Postre ["manzana" , "pera"] 20 90

applePie :: Postre
applePie = Postre ["manzana" , "leche"] 10 80

--b.

modificarTemperatura :: (Int -> Int) -> Postre -> Postre
modificarTemperatura funcion postre = postre {temperatura = funcion . temperatura $ postre}

modificarPeso :: (Float -> Float) -> Postre -> Postre
modificarPeso funcion postre = postre {peso = funcion . peso $ postre}

modificarSabores :: ([String] -> [String]) -> Postre -> Postre
modificarSabores funcion postre = postre {sabores = funcion . sabores $ postre}

type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio  = modificarTemperatura (+1) . perderPeso 0.95 

immobulus :: Hechizo
immobulus = modificarTemperatura (const 0)

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = anadirSabor ("concentrado") . perderPeso 0.9 

perderPeso :: Float -> Hechizo
perderPeso numero = modificarPeso (*numero) 

diffindo :: Float -> Hechizo
diffindo  = perderPeso

riddiculus :: String -> Hechizo
riddiculus sabor = modificarSabores (reverse) . anadirSabor sabor

anadirSabor sabor = modificarSabores ( sabor :)

-- riddikulus sabor postre = postre {sabores=(reverse sabor):sabores postre}

avadaKavadra :: Hechizo
avadaKavadra = immobulus . modificarSabores (const [])

-- c.
estanListos::Hechizo -> [Postre] ->Bool
estanListos hechizo = all (estaListo hechizo)

--hacerHechizoSobreListaPostres :: [Postre] -> Hechizo -> Postre
hacerHechizoSobreListaPostres postres hechizo = foldr ($)  hechizo postres 

--hacerHechizoSobrePostre postre hechizo = hechizo postre

estaListo :: Hechizo -> Postre -> Bool
estaListo hechizo postre = ((>0).peso) (hechizo postre) && ((>0).length.sabores) (hechizo postre) && ((>0).temperatura) (hechizo postre)

-- d-
--pesoPromedioListos::Hechizo -> [Postre] -> Float
--pesoPromedioListos hechizo = promedio . map peso. filter (estaListo hechizo)

--promedio :: [Float] -> Float
--promedio pesos 
 --   | length pesos > 0 = sum pesos / (length pesos)
 --   | otherwise = 0

--- Punto 02 ---

data Mago = Mago {

    hechizos :: [Hechizo],
    horrocrux :: Int

} deriving (Show)

modificarHechizos :: ([Hechizo] -> [Hechizo]) -> Mago -> Mago
modificarHechizos funcion mago = mago {hechizos = funcion . hechizos $ mago}

modificarHorrocrux :: (Int -> Int) -> Mago -> Mago
modificarHorrocrux funcion mago = mago {horrocrux = funcion . horrocrux $ mago}

anadirHechizo :: Hechizo -> Mago -> Mago
anadirHechizo hechizo = modificarHechizos ( hechizo :)

hermioneGranger = Mago {hechizos=[wingardiumLeviosa],horrocrux=1}

--a.

--practicarHechizo:: Hechizo -> Postre -> Mago-> Mago 
practicarHechizo hechizo postre mago
    | (hechizo postre) == avadaKavadra postre = anadirHechizo (hechizo) . modificarHorrocrux (+1)
    | otherwise = anadirHechizo (hechizo)

--B) 
mejorHechizo::Postre->Mago->Hechizo
mejorHechizo postre = compararSabores postre.hechizos

compararSabores::Postre->[Hechizo]->Hechizo 
compararSabores _ [unHechizo] = unHechizo
compararSabores postre (hechizo1:hechizo2:hechizos) 
    | (length.sabores)(hechizo1 postre) >= (length.sabores) (hechizo2 postre) =  compararSabores postre (hechizo1:hechizos)
    | otherwise = compararSabores postre (hechizo2:hechizos)

--3. Infinita Magia
--A)
infinitosPostres::Mesa
infinitosPostres = cycle conjuntoEnLaMesa

magoDeInfinitosHechizos::Mago
magoDeInfinitosHechizos = hermioneGranger{hechizos=cycle (hechizos hermioneGranger)}

--B) 
algunoDejaListo::Hechizo->Mesa->Bool
algunoDejaListo hechizo = any (estaListo hechizo)

{-Sí, existe alguna consulta que pueda hacer para que me sepa dar una respuesta, pero dependerá del hechizo que se elija para el conjunto
infinito de postres.

La lista infinita de postres construida en el punto A) repite el conjunto de postres formado por el bizcochoBorracho y la tarta de melaza. 
De puntos anteriores se sabía, por ejemplo, que con el hechizo incendio ambos quedan listos y que utilizando ridikulus "nomil" solo el bizcocho
queda listo. (Recordando que estar listo implica tener un peso no despreciable, no estar congelado y tener al menos un sabor).

En ambos casos, el primer elemento de la lista cumple la condición con estos hechizos mencionados (en el incendio el segundo elemento también, pero en
este caso no resulta relevante). Como Haskell trabaja con "call by value"y por Evaluación Diferida, al identificar que se trata de un any (es decir 
que si el hechizo pasado por parámetro deja listo al menos a un postre el resultado es True), no necesita seguir evaluando
las condiciones de los siguientes postres. 

En conclusión, si yo pregunto algunoDejaListo hechizo infinitosPostres (reemplazando hechizo por cualquiera que ya se sepa de antemano que hará
que al menos el primer elemento cumpla la condición), la consola sabe dar una respuesta; y la respuesta que da es siempre True.
  -}

--C)
{- Suponiendo que se tiene a un mago con infinitos hechizos, no existe ningún caso en el cual se pueda encontrar al mejor hechizo.
Cuando se habla de mejor, se describe una comparación para obtener de entre una lista de elementos el "máximo". En este contexto, 
se entiende al mejor hechizo como el que deja a un postre con la mayor cantidad de sabores.

Al escribir esta función, se la describe como una comparación recursiva de los sabores de un postre con los distintos hechizos del mago. 
Se toma el primer hechizo y se compara la cantidad de sabores del postre con dicho hechizo con la longitud de la lista de sabores resultado de 
aplicarle el segundo hechizo de la lista. En este caso, se queda con el que hace que haya más sabores en el postre, y retoma la evaluación. 
Cuando se trata de una lista de un único elemento, entonces arroja ese hechizo porque se entiende que ya hizo todas las comparaciones pertinentes
o que en la lista de hechizos había un único hechizo (caso base de la función). 

Al tratarse de una lista de infinitos hechizos, la consola no puede llegar al caso base mencionado, buscará comparar infinitamente cómo quedan los
sabores luego de aplicarle el hechizo al postre dado. Sin embargo, por el tipo de la función reconoce que el resultado que arrojaría es una función???

En resumen, dada cualquier forma de generar listas infinitas (repeat, cycle), en ningún caso se podrá encontrar al mejor hechizo pues se trata
de una comparación recursiva.
-}