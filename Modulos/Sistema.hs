import Modulos.Habitaciones
import Modulos.Info
import Modulos.Reservacion
module Modulos.Sistema where
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


menuPrincipal :: Sistema -> IO ()
menuPrincipal s = do
    putStrLn "Sistema gestor de hoteles\n"
    putStrLn "============================\n"
    putStrLn "Seleccione una opción:\n"
    putStrLn "1. Administrativo\n2. General\n3. Salir"
    putStrLn "============================\n"

    op <- getLine
    case op of
        "1" -> menuAdmin s   >>= menuPrincipal
        "2" -> menuGeneral s >>= menuPrincipal
        "3" -> putStrLn "Adiós."
        _   -> putStrLn "Opción inválida." >> menuPrincipal s