module Lib where
import Text.Show.Functions

--- Punto 01---
data Chofer = Chofer {
    nombreChofer :: String,
    kilometraje :: Int,
    viajes :: [Viaje],
    condicion :: Condicion
} deriving (Show)

data Cliente = Cliente {
    nombreCliente :: String,
    direccion :: String
} deriving (Show)

data Viaje = Viaje {
    fecha :: String,
    cliente :: Cliente,
    costo :: Int
} deriving (Show)

type Condicion = Viaje -> Bool

--- Punto 02 ---

cualquier :: Condicion
cualquier _ = True

viajeCaro :: Condicion
viajeCaro  = (>200) . costo

-- viajeCaro viaje = costo viaje > 200 

masDeNLetras :: Int -> Condicion
masDeNLetras n = (>n) . length . nombreCliente . cliente

noVivirZona :: String -> Condicion
noVivirZona zona = (/= zona) . direccion . cliente

-- (/= donde) . lugar . cliente
---     all (/= zona) (direccion cliente)
--- not . elem  zona (direccion cliente)

--- Punto 03 ---

--a.
lucas :: Cliente
lucas = Cliente "Lucas" "Victoria"

--b.
daniel :: Chofer
daniel = Chofer "Daniel" 23500 [Viaje "20/04/2017" lucas 150 ] (noVivirZona "Olivos")

--c. 
alejandra :: Chofer
alejandra = Chofer "Alejandra" 180000 [] cualquier

--- Punto 04 ---
choferPuede :: Viaje -> Chofer -> Bool
choferPuede viaje chofer = condicion chofer $ viaje

--- condicon chofer = condicion chofer
--- foldr ($) viaje chofer
--- viaje chofer = condicion chofer $ viaje

--- Punto 05 ---
liquidacionChofer :: Chofer -> Int
liquidacionChofer  = sum . map costo . viajes

--- Punto 06 ---
--realizarUnViaje :: Viaje -> [Chofer] -> Chofer
--realizarUnViaje viaje = efectuarViaje . choferConMenosViajes . filter (choferPuede viaje)

--choferConMenosViajes :: [Chofer] -> Chofer
--choferConMenosViajes [chofer] = chofer
--choferConMenosViajes (chofer1:chofer2:choferes) = choferConMenosViajes ((elQueMenosViajesHizo chofer1 chofer2):choferes)

--efectuarViaje viaje chofer = chofer { viajes = viaje : viajes chofer }

--elQueMenosViajesHizo :: Chofer -> Chofer -> Chofer
--elQueMenosViajesHizo chofer1 chofer2
 --  | cuantosViajes chofer1 > cuantosViajes chofer2 = chofer2
 --  | otherwise                                     = chofer1

--cuantosViajes = length . viajes

--- Punto 07 ---

repetirViaje viaje = viaje : repetirViaje viaje

nito :: Chofer
nito = Chofer "Nifo Infy" 70000 viajeInfinito $ masDeNLetras 3

viajeInfinito = repetirViaje $ Viaje "11/03/2017" lucas 50

--b. No es imposible saber la liquidacion pq se queda infinitamente calculando
--c. Si se puede saber 
