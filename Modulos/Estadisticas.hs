module Modulos.Estadisticas where
import Data.Time.Calendar (toGregorian, dayOfWeek)
import Modulos.Reservacion 
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Map as Map

reservaActivas :: Map.Map Int Reserva -> [Reserva]
reservaActivas = filter ((/= Cancelada) . reservaEstado) . Map.elems

totalHuespedes :: Reserva -> Int
totalHuespedes r = reservaAdulos r + reservaNinos r

diaDeLaSemana :: Int -> String
diaDeLaSemana n = case n of
    1 -> "Lunes"
    2 -> "Martes"
    3 -> "Miercoles"
    4 -> "Jueves"
    5 -> "Viernes"
    6 -> "Sabado"
    7 -> "Domingo"
    _ -> "Dia invalido"

ocupacionPorDia :: [Reserva] -> [(String, Int)]
ocupacionPorDia reservas =
    [(diaDeLaSemana (fromEnum (dayOfWeek d)), length (reservaHabitacionOcupada r))
    | r <- reservas, d <- nochesEntre (reservaEntrada r) (reservaSalida r)]

sumarTotalPorDia :: [Reserva] -> Map.Map String Int
sumarTotalPorDia = Map.fromListWith (+) .ocupacionPorDia

ordenarPorTotal :: Map.Map String Int -> [(String, Int)]
ordenarPorTotal = sortBy(comparing (Down . snd)) . Map.toList

mostrarTop3 :: Map.Map Int Reserva -> IO ()
mostrarTop3 reservas =
    case take 3 (ordenarPorTotal (sumarTotalPorDia (reservaActivas reservas))) of
        [] -> putStrLn "No hay ocupaciones registradas."
        top -> do
            putStrLn "Top 3 de dias con mas ocupaciones:"
            mapM_ (\(i, (dia, total)) -> putStrLn ("  " ++ show i ++ ". " ++ dia ++ ": " ++ show total ++ " habitaciones ocupadas"))
                  (zip [1 :: Int ..] top)

huespedesPorMesAnio :: Map.Map Int Reserva -> [((Integer, Int), Int)]
huespedesPorMesAnio reservas =
    Map.toList (Map.fromListWith (+)
        [ (mesAnio (reservaEntrada r), totalHuespedes r) | r <- reservaActivas reservas ])
  where
    mesAnio d = let (a, m, _) = toGregorian d in (a, m)

mostrarHuespedesPorMesAnio :: Map.Map Int Reserva -> IO ()
mostrarHuespedesPorMesAnio reservas =
    case huespedesPorMesAnio reservas of
        [] -> putStrLn "No hay reservaciones registradas."
        datos -> do
            putStrLn "Total de huespedes por mes-anio (segun fecha de entrada):"
            mapM_ (\((a, m), total) -> putStrLn ("  " ++ dosDigitos m ++ "-" ++ show a
                                                 ++ ": " ++ show total ++ " huespedes")) datos
  where
    dosDigitos m = if m < 10 then '0' : show m else show m

montoRecaudado :: Map.Map Int Reserva -> Double
montoRecaudado reservas = sum [reservaSubtotal r * 1.13 | r <- reservaActivas reservas]