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


mainSistema :: IO()
mainSistema = do 
    putStrLn "Sistema gestor de hoteles"