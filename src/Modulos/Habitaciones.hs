-- module Modulos.Habitaciones where
-- import Modulos.Auxiliares as Aux
-- import Data.List
-- import System.IO
-- import Control.Exception (catch, IOException)
-- import Distribution.Compat.Prelude (readMaybe)

module Modulos.Habitaciones where
import Control.Exception (catch, IOException)
import Data.List
import System.IO
import Text.Read (readMaybe)
import qualified Modulos.Auxiliares as Aux
-- import Distribution.Compat.Prelude (reads)


data TipoHabitacion = TipoHabitacion {
    tipo :: String,
    descripcion :: String,
    cantidadHuespedes :: Int
} deriving (Show, Read, Eq, Ord)

data Habitacion = Habitacion {
    idHabitacion :: Int,
    tipoHabitacion :: String,
    ocupada :: Bool
} deriving (Show, Read, Eq, Ord)

data HabitacionOcupada = HabitacionOcupada
  { habitacionOcupadaId :: Int
  , habitacionOcupadaTipo :: String
  , habitacionOcupadaAdultos :: Int
  , habitacionOcupadaNinos :: Int
  } deriving (Show, Read, Eq)

parsearLinea :: String -> Maybe TipoHabitacion
parsearLinea linea =
    case Aux.separarPor ',' linea of
        [t, d, c] -> TipoHabitacion t d <$> readMaybe c
        _         -> Nothing

listaTiposHabitaciones :: FilePath -> IO [TipoHabitacion]
listaTiposHabitaciones ruta = do
    contenido <- readFile ruta
    let lineas = filter (not . null) (lines contenido)
    putStrLn "Se formateo el archivo."
    return [x | Just x <- map parsearLinea lineas]

crearHabitaciones :: Int -> TipoHabitacion -> Int -> [Habitacion]
crearHabitaciones inicio t n = [Habitacion i (tipo t) False | i <- [inicio .. inicio + n - 1]]

validarTipoHabitacion :: [TipoHabitacion] -> String -> Bool
validarTipoHabitacion tipos cadena = any (\t -> tipo t  == cadena) tipos

validarTiposHabitaciones :: [TipoHabitacion] -> TipoHabitacion -> [TipoHabitacion]
validarTiposHabitaciones tipos nuevoTipo 
    | validarTipoHabitacion tipos (tipo nuevoTipo) = tipos
    | otherwise = tipos ++ [nuevoTipo]

mostrarHabitaciones :: [Habitacion] -> IO()
mostrarHabitaciones habitaciones = do
    putStrLn "Lista de habitaciones:\n"
    mapM_ (\x -> putStrLn $ "ID: " ++ show (idHabitacion x) ++ ", Tipo: " ++ tipoHabitacion x ++ "\n") habitaciones

validarRuta :: String -> IO Bool
validarRuta ruta = do
    (readFile ruta >> return True) `catch` (\e -> do
        let _ = e :: IOException
        return False)

cargarArchivo :: Bool -> [TipoHabitacion] -> FilePath -> IO [TipoHabitacion]
cargarArchivo existe tipos ruta = 
    if existe then do 
        nuevos <- listaTiposHabitaciones ruta
        let filtrarTipos = foldl validarTiposHabitaciones tipos nuevos
        putStrLn ("Tipos cargados: " ++ show (length filtrarTipos))
        return filtrarTipos
    else do
        putStrLn "Ruta no válida. Intente de nuevo."
        return tipos

ingresarCantidad :: TipoHabitacion -> IO Int
ingresarCantidad t = do
    putStr ("Cantidad de habitaciones tipo '" ++ tipo t ++ "': ")
    hFlush stdout
    cantidad <- getLine
    case readMaybe cantidad :: Maybe Int of
        Just n | n >= 0 -> return n
        _ -> do
            putStrLn "Cantidad inválida. Intente de nuevo."
            ingresarCantidad t

asignarCantidadPorTipo :: [TipoHabitacion] -> IO [Habitacion]
asignarCantidadPorTipo tipos = do
    grupos <- mapM (\t -> crearHabitaciones 1 t <$> ingresarCantidad t) tipos
    return (renumerar (concat grupos))

renumerar :: [Habitacion] -> [Habitacion]
renumerar = zipWith (\i h -> h { idHabitacion = i }) [1..]

    
menuHabitaciones :: [TipoHabitacion] -> [Habitacion] -> IO()
menuHabitaciones tipos habitaciones = do
    putStrLn "Bienvenido al hotel."
    putStrLn "¿Qué deseas hacer?"
    putStrLn "1. Cargar tipo de habitaciones"
    putStrLn "2. Asignar Cantidad de habitaciones por tipo"
    putStrLn "3. Salir"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese una ruta: \n"
            ruta <- getLine
            existe <- validarRuta ruta
            nuevosTipos <- cargarArchivo existe tipos ruta
            menuHabitaciones nuevosTipos habitaciones

        "2" 
            | not (null habitaciones) -> do
            putStrLn "Las habitaciones ya fueron generadas. No se puede repetir."
            menuHabitaciones tipos habitaciones
            | null tipos -> do
            putStrLn "Primero debes cargar los tipos de habitación."
            menuHabitaciones tipos habitaciones
            | otherwise -> do
            putStrLn "| CANTIDAD POR TIPO | \n"
            generadas <- asignarCantidadPorTipo tipos
            mostrarHabitaciones generadas
            menuHabitaciones tipos generadas

        "3" -> do
            putStrLn "Saliendo..."
        _   -> do
            putStrLn "Opción no válida. Intenta de nuevo."
            menuHabitaciones tipos habitaciones

mainHabitaciones ::  IO()
mainHabitaciones = menuHabitaciones [] []