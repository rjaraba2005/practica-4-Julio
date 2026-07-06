module RushHour where

import Data.List (nub, sort, intercalate)

-- La posicion en el tablero se representa con (fila, columna)
type Posicion = (Int, Int)

-- Un vehiculo es horizontal si ocupa casillas en la misma fila y vertical si ocupa casillas en la misma columna.
data Orientacion = Horizontal | Vertical
  deriving (Show, Eq)

-- Coche (longitudud 2) o camion (longitud 3).
data Vehiculo = Vehiculo {
    idVehiculo :: Char,       -- letra que lo identifica 
    longituditud :: Int,  
    orientacion :: Orientacion, 
    posicion :: Posicion      -- nodo clave del vehiculo, la celda mas a la izquierda o más arriba
} deriving (Show, Eq)

newtype Tablero = Tablero [Vehiculo]
  deriving (Eq)

instance Show Tablero where
    show (Tablero vehiculos) = "Tablero con " ++ show (length vehiculos) ++ " vehiculos: \n" ++ show vehiculos

--Divide una lista en grupos de tamaño n.
agruparEn :: Int -> [a] -> [[a]]
agruparEn _ [] = []
agruparEn n xs = take n xs : agruparEn n (drop n xs)

-- Convierte la cadena de 36 caracteres en una lista de (pos, caracter),
-- descartando las celdas vacias ('o').
cuadriculaDe :: String -> [(Posicion, Char)]
cuadriculaDe cadena =
    [ ((f, c), caracter)
    | (f, fila)     <- zip [0..5] (agruparEn 6 cadena)  
    , (c, caracter) <- zip [0..5] fila                  
    , caracter /= 'o' --se ignora                                  
    ]

-- Construye el Tablero completo
leerCadena :: String -> Tablero
leerCadena cadena = Tablero (map crearVehiculo idsVehiculos)
  where
    cuadricula = cuadriculaDe cadena

    -- Cada letra distinta es un vehiculo, usamos nub para quitar duplicados conservando el orden
    idsVehiculos = nub (map snd cuadricula)

    -- Reconstruye un vehiculo a partir de su letra
    crearVehiculo idCoche =
        -- Busca todas las celdas donde aparece esa letra
        let coords = sort [pos | (pos, coche) <- cuadricula, coche == idCoche]
        in if length coords < 2
           then error ("Vehiculo mal formado o incompleto: " ++ show idCoche)
           else
               let (f1, c1) = head coords -- Nodo clave 
                   (f2, c2) = coords !! 1 -- obtenemos también la segunda celda para comprobar si es horizontal o vertical
                   longitud = length coords  
                   orientacion = if f1 == f2 then Horizontal else Vertical 
               in Vehiculo idCoche longitud orientacion (f1, c1)

-- Comprueba que las celdas reales de cada vehiculo coincidan con la linea recta para asegurarnos de que los datos están bien
validarTablero :: String -> Tablero -> Bool
validarTablero cadena (Tablero vehiculos) =
    let errores = concat (map chequear vehiculos)
    in if null errores 
       then True
       else error ("Tablero invalido: " ++ intercalate "; " errores)
  where
    cuadricula = cuadriculaDe cadena
    chequear v =
        let real = sort [pos | (pos, c) <- cuadricula, c == idVehiculo v] -- Celdas que ocupa realmente el vehiculo
            reconstruido = sort (posicionesVehiculo v) -- Celdas que deberia ocupar
        in if real == reconstruido
           then []
           else ["Vehiculo " ++ [idVehiculo v] ++ " mal formado"]

-- Coge los primeros 36 caracteres de la entrada y llama a  leerCadena, el resto de la cadena se devuelve sin consumir
instance Read Tablero where
    readsPrec _ entrada =
        let (cadenaTablero, resto) = splitAt 36 entrada
        in if length cadenaTablero == 36
           then [(leerCadena cadenaTablero, resto)]
           else []


posicionesVehiculo :: Vehiculo -> [Posicion]
-- Calculamos que celdas ocupa el vehiculo usando su longitud
posicionesVehiculo (Vehiculo _ longitud Horizontal (f, c)) = [(f, c + i) | i <- [0 .. longitud - 1]]
posicionesVehiculo (Vehiculo _ longitud Vertical (f, c))   = [(f + i, c) | i <- [0 .. longitud - 1]]

enLimites :: Posicion -> Bool
enLimites (f, c) = f >= 0 && f < 6 && c >= 0 && c < 6

estaLibre :: Posicion -> [Vehiculo] -> Bool
estaLibre pos coches = not (any (\coche -> pos `elem` posicionesVehiculo coche) coches)

data DirMovimiento = Adelante | Atras deriving (Show, Eq)

-- Intenta hacer un movimiento para un vehiculo
intentarMover :: Vehiculo -> DirMovimiento -> Tablero -> Maybe Vehiculo
intentarMover (Vehiculo idCoche longitud orientacion (f, c)) dir (Tablero todosVehiculos) =
    let
        -- desplazamiento segun orientacion y direccion
        (df, dc) = case (orientacion, dir) of
            (Horizontal, Adelante) -> (0, 1)
            (Horizontal, Atras)    -> (0, -1)
            (Vertical, Adelante)   -> (1, 0)
            (Vertical, Atras)      -> (-1, 0)

        nuevaPos = (f + df, c + dc)
        vMovido = Vehiculo idCoche longitud orientacion nuevaPos -- vehiculo con el nodo clave desplazado

        -- El porpio vehiculo no cuenta como obstaculo para si mismo
        otrosVehiculos = filter (\coche -> idVehiculo coche /= idCoche) todosVehiculos

        -- El movimiento es valido si esta dentro de los limites y las casillas a las que se mueve estan libres
        esValido = all (\p -> enLimites p && estaLibre p otrosVehiculos) (posicionesVehiculo vMovido)
    in
        if esValido then Just vMovido else Nothing

-- Genera todos los tableros alcanzables en un unico movimiento desde "b":
siguientesTableros :: Tablero -> [Tablero]
siguientesTableros (Tablero vehiculos) =
    let b = Tablero vehiculos
    in [ Tablero (map (\coche -> if idVehiculo coche == idVehiculo v then vMovido else coche) vehiculos)
    | v <- vehiculos, 
    dir <- [Adelante, Atras],
    Just vMovido <- [intentarMover v dir b]   -- Se descartan los movimientos invalidos 
    ]


buscarCocheRojo :: Tablero -> Vehiculo
buscarCocheRojo (Tablero vehiculos) = head (filter (\v -> idVehiculo v == 'A') vehiculos)

-- El puzzle esta resuelto cuando el coche A tiene su nodo clave en la columna 4
estaResuelto :: Tablero -> Bool
estaResuelto tablero =
    let (Vehiculo _ _ _ (_, c)) = buscarCocheRojo tablero
    in c == 4

-- Cada tablero es un nodo que tiene una arista hacia cada tablero alcanzable en un movimiento.
resolver :: Tablero -> Maybe [Tablero]
resolver tableroInicial = bfs [[tableroInicial]] []
  where
    -- La cola contiene caminos completos desde el inicio ya que tenemos que guardalos entero para luego poder devolver la solución completa
    -- Visitados es la lista de tableros ya explorados, para no repetir trabajo.
    bfs :: [[Tablero]] -> [Tablero] -> Maybe [Tablero]
    bfs [] _ = Nothing
    bfs ((tableroActual:restoRuta):cola) visitados
        | estaResuelto tableroActual = Just (reverse (tableroActual:restoRuta)) 
        | tableroActual `elem` visitados = bfs cola visitados  -- Ya explorado, se descarta
        | otherwise =
            let siguientesOpciones = siguientesTableros tableroActual
                siguientesValidos = filter (`notElem` visitados) siguientesOpciones
                -- Cada opcion nueva forma un camino nuevo
                nuevasRutas = [sigTablero : (tableroActual:restoRuta) | sigTablero <- siguientesValidos]
            -- Las rutas nuevas se añaden al final de la cola
            in bfs (cola ++ nuevasRutas) (tableroActual : visitados)


data Dificultad = Principiante | Intermedio | Avanzado | Experto
  deriving (Show, Eq)

-- Numero de movimientos de la solucion minima encontrada.
pasosSolucion :: [Tablero] -> Int
pasosSolucion ruta = length ruta - 1   -- -1 porque la ruta incluye el tablero inicial

-- Numero de vehiculos que hay en el tablero.
totalVehiculos :: Tablero -> Int
totalVehiculos (Tablero vehiculos) = length vehiculos

-- Ids de los vehiculos que cambian de posicion en algun punto de la ruta
vehiculosMovidos :: [Tablero] -> [Char]
vehiculosMovidos ruta = nub (concat (zipWith difieren ruta (tail ruta)))
  where
    -- compara dos tableros consecutivos de la ruta y devuelve la letra del vehiculo
    difieren (Tablero vs1) (Tablero vs2) =
        [idVehiculo v2 | (v1, v2) <- zip vs1 vs2, posicion v1 /= posicion v2]

-- Porcentaje de vehiculos que se mueven para dar con la solucón minima
proporcionPiezasMovidas :: [Tablero] -> Float
proporcionPiezasMovidas ruta =
    let total = totalVehiculos (head ruta)
        movidos = length (vehiculosMovidos ruta)
    in if total == 0 then 0 else fromIntegral movidos / fromIntegral total

-- Grado de simetria de la situacion inicial: proporcion de celdas
-- Ocupadas cuya celda simetrica tambien esta ocupada, tomamos la simetría que sea mejor,
-- Por ejemplo si el tablero no es simetro horizontalmente pero si verticalmente escogemos la que tenga un mayor valor de simetría.
gradoSimetria :: Tablero -> Float
gradoSimetria (Tablero vehiculos) =
    let ocupadas = concat (map posicionesVehiculo vehiculos)   -- todas las celdas ocupadas por algun vehiculo
        total = length ocupadas
        reflejoH (f, c) = (f, 5 - c) -- espejo izquierda-derecha
        reflejoV (f, c) = (5 - f, c) -- espejo arriba-abajo
        rotacion180 (f, c) = (5 - f, 5 - c)  -- giro de 180 grados sobre el centro
        -- que fraccion de celdas ocupadas tiene tambien ocupada su posicion simetrica
        ratio transformar = fromIntegral (length (filter (\p -> transformar p `elem` ocupadas) ocupadas)) / fromIntegral total
    in if total == 0 then 0 else maximum [ratio reflejoH, ratio reflejoV, ratio rotacion180]

-- Clasifica la dificultad de un tablero a partir de su solucion minima, lod escalares de los polinomios los hemos editado a mano para 
-- que dieran resultados aproximados a lo que puede ser la dificultad de cada tablero
evaluarDificultad :: [Tablero] -> Dificultad
evaluarDificultad ruta =
    let
        tableroInicial = head ruta
        pasos = fromIntegral (pasosSolucion ruta) :: Float
        numVehiculos = fromIntegral (totalVehiculos tableroInicial) :: Float
        propMovidas = proporcionPiezasMovidas ruta
        simetria = gradoSimetria tableroInicial
        puntuacion = pasos
                   + 0.6 * numVehiculos
                   + 20  * propMovidas
                   + 8   * simetria
    in
        --estos umbrales los hemos puesto también por prueba y error. Más o menos acertamos en un 75% de las veces. 
        -- Pero distinguimos entre los más fáciles y los más difíciles, nos equivocamos cuando la dificultad es pareja
        if puntuacion < 68.0 then Principiante
        else if puntuacion < 72.0 then Intermedio
        else if puntuacion < 83.0 then Avanzado
        else Experto
