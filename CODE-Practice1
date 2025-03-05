import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    idEstudiante :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime
} deriving (Show, Read, Eq)

-- Función para registrar la entrada de un estudiante
registrarEntradaEstudiante :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEntradaEstudiante id tiempo estudiantes =
    Estudiante id tiempo Nothing : estudiantes

-- Función para registrar la salida de un estudiante
registrarSalidaEstudiante :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalidaEstudiante id tiempo estudiantes =
    case find (\e -> idEstudiante e == id) estudiantes of
        Just estudiante ->
            if isNothing (salida estudiante)
            then map (\e -> if idEstudiante e == id then e { salida = Just tiempo } else e) estudiantes
            else estudiantes
        Nothing -> estudiantes

isNothing :: Maybe UTCTime -> Bool
isNothing Nothing = True
isNothing _       = False

-- Función para buscar un estudiante por ID
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante id = find (\e -> idEstudiante e == id && isNothing (salida e))

-- Función para calcular el tiempo que un estudiante permaneció en la universidad
tiempoEnUniversidad :: Estudiante -> IO NominalDiffTime
tiempoEnUniversidad estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (entrada estudiante)

-- Función para guardar la información de los estudiantes en un archivo
guardarEstudiantes :: [Estudiante] -> IO ()
guardarEstudiantes estudiantes = do
    withFile "universidad.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarEstudiante estudiantes))
    putStrLn "Información guardada en el archivo universidad.txt."

-- Función para cargar los estudiantes desde un archivo
cargarEstudiantes :: IO [Estudiante]
cargarEstudiantes = do
    contenido <- withFile "universidad.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        contenido `deepseq` return contenido
    let lineas = lines contenido
    return (map leerEstudiante lineas)
    where
        leerEstudiante linea = read linea :: Estudiante

-- Función para mostrar la información de un estudiante
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante id entrada salida) =
    "Estudiante {ID = \"" ++ id ++ "\", entrada = " ++ show entrada ++ ", salida = " ++ maybe "Nothing" show salida ++ "}"

-- Función para listar los estudiantes en la universidad
listaDeEstudiantes :: [Estudiante] -> IO ()
listaDeEstudiantes [] = putStrLn "No Se encuentran estudiantes en la universidad."
listaDeEstudiantes estudiantes = do
    putStrLn "Estudiantes en la universidad:"
    mapM_ (putStrLn . mostrarEstudiante) estudiantes

-- Función principal del programa
main :: IO ()
main = do
    estudiantes <- cargarEstudiantes
    putStrLn "¡Bienvenido al Sistema de Gestión de Estudiantes!"

    -- Ciclo principal del programa
    cicloPrincipal estudiantes

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal estudiantes = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por ID"
    putStrLn "4. Lista de estudiantes"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del estudiante:"
            id <- getLine
            tiempoActual <- getCurrentTime
            let estudiantesActualizados = registrarEntradaEstudiante id tiempoActual estudiantes
            putStrLn $ "Estudiante con ID " ++ id ++ " ha ingresado a la universidad."
            guardarEstudiantes estudiantesActualizados
            cicloPrincipal estudiantesActualizados

        "2" -> do
            putStrLn "Ingrese el ID del estudiante a salir:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let estudiantesActualizados = registrarSalidaEstudiante idEstudiante tiempoActual estudiantes
            if estudiantesActualizados == estudiantes
                then putStrLn "Error: El estudiante no está registrado o ya ha salido previamente."
                else putStrLn $ "Estudiante con ID " ++ idEstudiante ++ " ha salido de la universidad."
            guardarEstudiantes estudiantesActualizados
            cicloPrincipal estudiantesActualizados

        "3" -> do
            putStrLn "Ingrese el ID del estudiante a buscar:"
            id <- getLine
            case buscarEstudiante id estudiantes of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnUniversidad estudiante
                    putStrLn $ "El estudiante con ID " ++ id ++ " se encuentra en la universidad."
                    putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la universidad."
            cicloPrincipal estudiantes

        "4" -> do
            listaDeEstudiantes estudiantes
            cicloPrincipal estudiantes

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida; por favor seleccione una de las opciones en pantalla."
            cicloPrincipal estudiantes
