module Modulos.Reservacion where
import Modulos.Habitaciones
import Data.Time.Calendar (Day)

data Reserva = Reserva {
    reservaId :: Int,
    reservaNombre :: String,
    reservaFechaHora :: String,
    reservaEntrada :: Day,
    reservaSalida :: Day,
    reservaAdulos :: Int,
    reservaNinos :: Int,
    reservaEstado :: EstadoReservacion,
    reservaHabitacionOcupada :: [HabitacionOcupada],
    reservaSubtotal :: Double
} deriving (Show, Read, Eq)

data EstadoReservacion = Activa | Facturada | Cancelada
    deriving (Show, Read, Eq, Ord)

mainReservacion :: IO()
mainReservacion = do 
    putStrLn "reservacion"