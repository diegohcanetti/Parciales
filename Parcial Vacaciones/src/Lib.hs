module Lib where
import Text.Show.Functions

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

--- Punto 01 ---

data Turista = Turista {
    idiomas :: [String],
    viajaSolo :: Bool,
    cansancio :: Int,
    estres :: Int
} deriving (Eq,Show)

ana :: Turista
ana = Turista ["espanol"] False 0 21

beto :: Turista
beto =  Turista { cansancio = 15, estres = 15, viajaSolo = True, idiomas = ["aleman"] }

cathi :: Turista
cathi = Turista { cansancio = 15, estres = 15, viajaSolo = True, idiomas = ["aleman", "catalan"] }

--- Punto 02 ---

type Excursion = Turista -> Turista
 
modificarCansancio :: Int -> Turista -> Turista
modificarCansancio delta turista = turista {cansancio = cansancio turista + delta }

modificarEstres :: Int -> Turista -> Turista
modificarEstres delta turista = turista {estres = estres turista + delta }

modificarEstresPorcentuaL :: Int -> Turista -> Turista
modificarEstresPorcentuaL porciento turista = modificarEstres (div (porciento * estres turista) 100) turista

modificarIdioma :: ([String] -> [String]) -> Turista -> Turista
modificarIdioma funcion turista = turista {idiomas = funcion (idiomas turista) }

acompaniado turista = turista {viajaSolo = False}

irPlaya :: Excursion
irPlaya  turista
    | viajaSolo turista = modificarCansancio (-5) turista
    | otherwise = modificarEstres (-1) turista

apreciarElementoPaisaje :: String -> Excursion
apreciarElementoPaisaje elemento  = modificarEstres (-length elemento) 

salirHablarIdioma :: String -> Excursion
salirHablarIdioma idioma  = modificarIdioma (idioma:) . acompaniado

caminar :: Int -> Excursion
caminar tiempo = modificarEstres (-intensidad tiempo) . modificarCansancio (intensidad tiempo) 

intensidad tiempo = div tiempo 4

data Marea = Tranquila | Moderada | Fuerte

paseoEnBarco :: Marea -> Excursion
paseoEnBarco Tranquila = acompaniado
paseoEnBarco Moderada  = id
paseoEnBarco Fuerte    = modificarCansancio 10 . modificarEstres 6

--a--
hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion = modificarEstresPorcentuaL (-10) . excursion 

--b--
deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun indice turista excursion = deltaSegun indice (hacerExcursion excursion turista) turista

-- c)
esEducativa :: Turista -> Excursion -> Bool
esEducativa turista = (> 0) . deltaExcursionSegun (length . idiomas) turista 

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista = filter (esDesestresante turista)

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista = (<= -3) . deltaExcursionSegun estres turista


-- 3)
type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciarElementoPaisaje "cascada", caminar 40, irPlaya, salidaLocal]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco Tranquila, excursion, caminar 120]

islaVecina :: Marea -> Tour
islaVecina mareaVecina = [paseoEnBarco mareaVecina, excursionEnIslaVecina mareaVecina, paseoEnBarco mareaVecina]

--
excursionEnIslaVecina :: Marea -> Excursion
excursionEnIslaVecina Fuerte = apreciarElementoPaisaje "lago"
excursionEnIslaVecina _  = irPlaya

salidaLocal :: Excursion
salidaLocal = salirHablarIdioma "melmacquiano"

-- a)
hacerTour :: Turista -> Tour -> Turista
hacerTour turista tour =
  foldl (flip hacerExcursion) (modificarEstres (length tour) turista) tour

-- b)
propuestaConvincente :: Turista -> [Tour] -> Bool
propuestaConvincente turista = any (esConvincente turista)

esConvincente :: Turista -> Tour -> Bool
esConvincente turista = any (dejaAcompaniado turista) . excursionesDesestresantes turista

dejaAcompaniado :: Turista -> Excursion -> Bool
dejaAcompaniado turista = not . viajaSolo . flip hacerExcursion turista

-- c)
efectividad :: Tour -> [Turista] -> Int
efectividad tour = sum . map (espiritualidadAportada tour) . filter (flip esConvincente tour)

espiritualidadAportada :: Tour -> Turista -> Int
espiritualidadAportada tour = negate . deltaRutina tour

deltaRutina :: Tour -> Turista -> Int
deltaRutina tour turista =
  deltaSegun nivelDeRutina (hacerTour turista tour) turista

nivelDeRutina :: Turista -> Int
nivelDeRutina turista = cansancio turista + estres turista


-- 4)
-- a)
playasEternas :: Tour
playasEternas = salidaLocal : repeat irPlaya

-- b)
{-
Para Ana sí porque la primer actividad ya es desestresante y siempre está acompañada.
Con Beto no se cumple ninguna de las 2 condiciones y el algoritmo diverge.
-}

-- c)
{-
No, solamente funciona para el caso que se consulte con una lista vacía de turista, que dará siempre 0.
-}
