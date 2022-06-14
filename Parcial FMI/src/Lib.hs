module Lib where
import Text.Show.Functions

--- Punto 01 ---
-- a.
type Recurso = String

data Pais = Pais {
    ingresoPerCapita :: Float,
    activosPublico :: Float,
    activosPrivado :: Float,
    recursosNaturales :: [Recurso],
    deuda :: Float
} deriving (Eq, Show)

-- b.

namibia :: Pais
namibia = Pais 4140 400000 650000 ["Mineria", "Ecoturismo"] 50

venezuela :: Pais
venezuela = Pais 4140 400000 650000 ["Mineria", "Ecoturismo"] 150
--- Punto 02 ---
type Receta = [Estrategia]
type Estrategia = Pais -> Pais

modificarDeuda :: (Float -> Float) -> Pais -> Pais
modificarDeuda funcion pais = pais {deuda = funcion . deuda $ pais}

modificarActivosPublicos :: (Float -> Float) -> Pais -> Pais
modificarActivosPublicos funcion pais = pais {activosPublico = funcion . activosPublico $ pais}

modificarIngresoPerCapita :: (Float -> Float) -> Pais -> Pais
modificarIngresoPerCapita funcion pais = pais {ingresoPerCapita = funcion . ingresoPerCapita $ pais}

modificarRecursos  :: ([Recurso] -> [Recurso]) -> Pais -> Pais
modificarRecursos funcion pais = pais {recursosNaturales = funcion . recursosNaturales $ pais}
--- prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta (por los intereses) 

prestarDineroPais :: Float -> Estrategia
prestarDineroPais cantidad = modificarDeuda (+ cobrarNumberereses cantidad)

cobrarNumberereses :: Float -> Float
cobrarNumberereses cuanto = cuanto * 1.5
--- reducir x cantidad de puestos de trabajo del sector público, lo que provoca que se reduzca la cantidad de activos en el sector público y 
--- además que el ingreso per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en caso contrario

reducirCantidadPuestosTrabajo :: Float -> Estrategia
reducirCantidadPuestosTrabajo cantidad =  modificarActivosPublicos (flip (-) cantidad) . modificarIngresoPerCapita ( * (1 - reduccionIngreso cantidad))

reduccionIngreso :: Float -> Float
reduccionIngreso cantidad 
    |cantidad > 100              = 0.2
    |otherwise                   = 0.15

---  disminuye 2 millones de dólares la deuda que el país mantiene con el FMI  pero también deja momentáneamente sin recurso natural a dicho país. 
--- No considerar qué pasa si el país no tiene dicho recurso.

explotar :: Recurso -> Estrategia
explotar recurso = modificarDeuda (flip (-) 2) . modificarRecursos (quitarRecurso recurso)

quitarRecurso :: Recurso -> [Recurso] -> [Recurso]
quitarRecurso recurso = filter (/= recurso)

--- establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su Producto Bruto Interno 
--- (que se calcula como el ingreso per cápita multiplicado por su población activa, sumando puestos públicos y privados de trabajo) y reducir 500 puestos de trabajo del sector público. 

blindaje :: Estrategia
blindaje pais = prestarDineroPais (productoBrutoInterno pais * 0.5) . (reducirCantidadPuestosTrabajo 500) $ pais

productoBrutoInterno :: Pais -> Float
productoBrutoInterno pais = ingresoPerCapita pais * poblacionActiva pais 

poblacionActiva :: Pais -> Float
poblacionActiva pais = activosPrivado pais + activosPublico pais


--- Punto 03 ---
receta :: Receta
receta = [prestarDineroPais 2000, explotar "Mineria"]

aplicarReceta :: Pais -> Receta -> Pais
aplicarReceta = foldr ($) 

--- Punto 04 ---

puedeZafar :: [Pais] -> [Pais]
puedeZafar = filter (elem "Petroleo" . recursosNaturales)

deudaPaises :: [Pais] -> Float
deudaPaises = sum . map deuda

--- Punto 05 ---
--estaOrdenado :: Pais -> [Receta] -> Bool
--estaOrdenado pais [receta] = True
--estaOrdenado pais (receta1:receta2:recetas) = revisarPBI receta1 pais <= revisarPBI receta2 pais && estaOrdenado pais (receta2:recetas)
   --  where revisarPBI receta = productoBrutoInterno . aplicarReceta receta