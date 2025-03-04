import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq  -- Importamos deepseq para forzar evaluación

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    identificacion :: String,
    ingreso :: UTCTime,
    salida :: Maybe UTCTime  
} deriving (Show, Read)

-- Función para registrar el ingreso de un estudiante a la universidad
registrarIngreso :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarIngreso idEstudiante tiempo universidad =
    Estudiante idEstudiante tiempo Nothing : universidad

-- Función para registrar la salida de un estudiante de la universidad
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida idEstudiante tiempo universidad =
    map (\e -> if idEstudiante == identificacion e then e { salida = Just tiempo } else e) universidad

-- Función para buscar un estudiante por su identificación en la universidad
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante idEstudiante universidad =
    find (\e -> idEstudiante == identificacion e && isNothing (salida e)) universidad
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un estudiante permaneció en la universidad
tiempoEnUniversidad :: Estudiante -> IO NominalDiffTime
tiempoEnUniversidad estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (ingreso estudiante)

-- Nueva función para calcular el tiempo de un estudiante
calcularTiempoEstudiante :: String -> [Estudiante] -> IO ()
calcularTiempoEstudiante idEstudiante universidad = do
    case find (\e -> identificacion e == idEstudiante) universidad of
        Just estudiante -> do
            tiempoTotal <- tiempoEnUniversidad estudiante
            putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
        Nothing -> putStrLn "Estudiante no encontrado en la universidad."

-- Función para guardar la información de los estudiantes en un archivo de texto
guardarUniversidad :: [Estudiante] -> IO ()
guardarUniversidad universidad = do
    withFile "universidad.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarEstudiante universidad))
    putStrLn "Información guardada en el archivo universidad.txt."

-- Función para cargar la información de los estudiantes desde un archivo de texto
cargarUniversidad :: IO [Estudiante]
cargarUniversidad = do
    contenido <- readFile "universidad.txt"
    contenido `deepseq` return (map leerEstudiante (lines contenido))  -- Forzamos evaluación con deepseq
    where
        leerEstudiante linea = read linea :: Estudiante

-- Función para mostrar la información de un estudiante como cadena de texto
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante id ingreso salida) =
    "Estudiante {identificacion = \"" ++ id ++ "\", ingreso = " ++ show ingreso ++ ", salida = " ++ maybe "Nothing" show salida ++ "}"

-- Función para listar los estudiantes en la universidad
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes en la universidad."
listarEstudiantes estudiantes = do
    putStrLn "Estudiantes en la universidad:"
    mapM_ (putStrLn . mostrarEstudiante) estudiantes

-- Función principal del programa
main :: IO ()
main = do
    universidad <- cargarUniversidad
    putStrLn "¡Bienvenido al Sistema de Gestión Universitaria!"
    cicloPrincipal universidad

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal universidad = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar ingreso de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por identificación"
    putStrLn "4. Listar estudiantes"
    putStrLn "5. Calcular tiempo de un estudiante"
    putStrLn "6. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la identificación del estudiante:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizada = registrarIngreso idEstudiante tiempoActual universidad
            putStrLn $ "Estudiante con identificación " ++ idEstudiante ++ " ingresó a la universidad."
            guardarUniversidad universidadActualizada
            cicloPrincipal universidadActualizada

        "2" -> do
            putStrLn "Ingrese la identificación del estudiante a salir:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizada = registrarSalida idEstudiante tiempoActual universidad
            putStrLn $ "Estudiante con identificación " ++ idEstudiante ++ " salió de la universidad."
            guardarUniversidad universidadActualizada
            cicloPrincipal universidadActualizada

        "3" -> do
            putStrLn "Ingrese la identificación del estudiante a buscar:"
            idEstudiante <- getLine
            case buscarEstudiante idEstudiante universidad of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnUniversidad estudiante
                    putStrLn $ "El estudiante con identificación " ++ idEstudiante ++ " se encuentra en la universidad."
                    putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la universidad."
            cicloPrincipal universidad

        "4" -> do
            listarEstudiantes universidad
            cicloPrincipal universidad

        "5" -> do
            putStrLn "Ingrese la identificación del estudiante:"
            idEstudiante <- getLine
            calcularTiempoEstudiante idEstudiante universidad
            cicloPrincipal universidad

        "6" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal universidad
