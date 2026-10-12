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
    mapM_ (\x -> putStrLn $ "ID: " ++ show (idHabitacion x) ++ ", Tipo: " ++ tipoHabitacion x ++ "\n") habitaciones

validarRuta :: String -> IO Bool
validarRuta ruta = do
    (readFile ruta >> return True) `catch` (\e -> do
        let _ = e :: IOException
        return False)

cargarArchivo existe ruta = 
    if existe then do 
        tipos <- listaTiposHabitaciones ruta
        putStrLn "Ruta valida. Procesando archivo..." 
    else putStrLn "Ruta no válida. Intenta de nuevo."
        
validarCantidad :: String -> IO ()
validarCantidad cantidad =
    if (readMaybe cantidad :: Maybe Int) == Nothing then do
        putStrLn "Cantidad no válida. Intenta de nuevo."
        mainHabitaciones
    else if read cantidad < 0 then do 
        putStrLn "Cantidad no válida. Intenta de nuevo."
        
    else do
        putStrLn "Cantidad válida. Procesando..."
        

mainHabitaciones :: IO()
mainHabitaciones = do
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
            cargarArchivo existe ruta
            mainHabitaciones
        "2" -> do
            putStrLn "Ingrese la cantidad de habitaciones por tipo: \n"
            cantidad <- getLine
            validarCantidad cantidad
        "3" -> do
            putStrLn "Saliendo..."
        _   -> do
            return ()
