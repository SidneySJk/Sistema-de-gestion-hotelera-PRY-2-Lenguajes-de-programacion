module Modulos.Sistema where
import Modulos.Habitaciones
import Modulos.Info
import Modulos.Reservacion
import qualified Data.Map as Map

data Sistema = Sistema
  { sistemaHotel :: Maybe InfoHotel
  , sistemaTipos :: Map.Map String TipoHabitacion
  , sistemaHabitaciones :: [Habitacion]
  , tarifasSistema  :: Map.Map Int Double     
  , reservasSistema :: Map.Map Int Reserva
  , sigIdReserva :: Int
  , sigIdFactura :: Int
  } deriving (Show, Read)

sistemaInicial :: Sistema
sistemaInicial = Sistema Nothing Map.empty [] Map.empty Map.empty 1 1

menuPrincipal :: Sistema -> IO ()
menuPrincipal s = do
    putStrLn "\nSistema gestor de hoteles\n"
    putStrLn "============================\n"
    putStrLn "Seleccione una opción:\n"
    putStrLn "1. Menú Administrativo\n2. Menú General\n3. Salir \n"
    putStrLn "============================\n"

    putStr "Ingrese su opción: "
    op <- getLine
    case op of
        "1" -> menuAdmin s   >>= menuPrincipal
        "2" -> menuGeneral s >>= menuPrincipal
        "3" -> putStrLn "Adiós."
        _   -> putStrLn "Opción inválida." >> menuPrincipal s

menuAdmin :: Sistema -> IO Sistema
menuAdmin s = do
    putStrLn "(menú administrativo pendiente)"
    return s

menuGeneral :: Sistema -> IO Sistema
menuGeneral s = do
    putStrLn "(menú general pendiente)"
    return s