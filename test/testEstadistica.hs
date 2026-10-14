import qualified Data.Map as Map
import Data.Time.Calendar (fromGregorian)
import Modulos.Habitaciones (HabitacionOcupada(..))
import Modulos.Reservacion
import Modulos.Estadisticas

reservasDePrueba :: Map.Map Int Reserva
reservasDePrueba = Map.fromList [(1, r1), (2, r2)]
  where
    h i = HabitacionOcupada i "Estandar" 2 1
    r1 = Reserva 1 "Ana" "2026-03-20 10:00:00"
                 (fromGregorian 2026 3 29) (fromGregorian 2026 4 2)
                 2 1 Activa [h 1, h 2] 1000
    r2 = Reserva 2 "Luis" "2026-03-21 11:00:00"
                 (fromGregorian 2026 4 1) (fromGregorian 2026 4 3)
                 1 0 Facturada [h 1] 500

main :: IO ()
main = mostrarTop3 reservasDePrueba