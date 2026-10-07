module Modulos.Habitaciones where
import Data.List
import System.IO
import Modulos.Auxiliares as Aux
import Control.Exception (catch, IOException)
import Distribution.Compat.Prelude (readMaybe)

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

crearHabitaciones :: TipoHabitacion -> Int -> [Habitacion]
crearHabitaciones t cantidadHabitaciones = do
    [Habitacion i (tipo t) False | i <- [1..cantidadHabitaciones]]

validarTipoHabitacion :: [TipoHabitacion] -> String -> Bool
validarTipoHabitacion tipos cadena = any (\t -> tipo t  == cadena) tipos

validarTiposHabitaciones :: [TipoHabitacion] -> TipoHabitacion -> [TipoHabitacion]
validarTiposHabitaciones tipos nuevoTipo 
    | validarTipoHabitacion tipos (tipo nuevoTipo) = tipos
    | otherwise = tipos ++ [nuevoTipo]

mostrarHabitaciones :: [Habitacion] -> IO()
mostrarHabitaciones habitaciones = do
    putStrLn "Lista de habitaciones:\n"
    map (\x -> putStrLn $ "ID: " ++ show (idHabitacion x) ++ ", Tipo: " ++ tipoHabitacion x ++ "\n") habitaciones


mainHabitaciones :: IO()
mainHabitaciones = do
    putStrLn "Bienvenido al hotel."
    putStrLn "¿Qué deseas hacer?"
    putStrLn "1. Mirar alrededor"
    putStrLn "2. Abrir la puerta"
    putStrLn "3. Salir"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese una ruta: \n"
            ruta <- getLine
            tipos <- listaTiposHabitaciones ruta
            putStrLn "M"
            mainHabitaciones        
        _   -> do
            putStrLn "Opción no válida. Intenta de nuevo."
            mainHabitaciones
