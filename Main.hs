module Main where

import RushHour

-- Analiza un único tablero y muestra el resultado
analizarTablero :: String -> IO ()
analizarTablero cadenaTablero = do
    putStrLn $ "\nAnalizando tablero: " ++ take 36 cadenaTablero
    let tableroBruto = leerCadena cadenaTablero

    if validarTablero cadenaTablero tableroBruto 
        then do
            case resolver tableroBruto of
                Nothing -> putStrLn "No se ha encontrado solución para este tablero."
                Just ruta -> do
                    let pasos = pasosSolucion ruta
                    let piezas = totalVehiculos tableroBruto
                    let dificultad = evaluarDificultad ruta

                    putStrLn $ "Solucionado en " ++ show pasos ++ " pasos."
                    putStrLn $ "Número de piezas: " ++ show piezas
                    putStrLn $ "Clasificación calculada: " ++ show dificultad
        else 
            -- Caso en que el tablero no es valido
            putStrLn "Error: Tablero inválido."

main :: IO ()
main = do
    contenido <- readFile "RushHour.txt"
    -- Con el filtro de longitud y obligando a no coger las lineas con '-' nos quitamos los textos de la dificultad
    let lineasValidas = filter (\l -> length l == 36 && notElem '-' l) (lines contenido)

    mapM_ analizarTablero lineasValidas

    putStrLn "Fin del análisis."