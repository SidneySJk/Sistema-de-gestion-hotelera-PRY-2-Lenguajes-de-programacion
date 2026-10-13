module Modulos.Reservacion where
import Modulos.Habitaciones
import Modulos.Info
import Modulos.Auxiliares (separarPor)
import Data.Time.Calendar
import Text.Read (readMaybe)
import qualified Data.Map as Map

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

cubreDia :: Day -> Reserva -> Bool
cubreDia d r = reservaEntrada r <= d && d < reservaSalida r

ocupadaEn :: Day -> Int -> Map.Map Int Reserva -> Maybe Int
ocupadaEn d idHab reservas =
    case filter ocupa (Map.elems reservas) of
        (r:_) -> Just (reservaId r)
        []    -> Nothing
  where
    ocupa r = reservaEstado r /= Cancelada
           && cubreDia d r
           && any (\h -> habitacionOcupadaId h == idHab) (reservaHabitacionOcupada r)

nochesEntre :: Day -> Day -> [Day]
nochesEntre entrada salida = [entrada .. addDays (-1) salida]

libreEnRango :: Day -> Day -> Int -> Map.Map Int Reserva -> Bool
libreEnRango entrada salida idHab reservas =
    all (\d -> ocupadaEn d idHab reservas == Nothing) (nochesEntre entrada salida)

codigoTarifa :: Day -> Bool -> Int
codigoTarifa d esAdulto = base + finDe + nino
  where
    (_, mes, _) = toGregorian d
    base  = if mes >= 11 || mes <= 3 then 5 else 1
    finDe = if dayOfWeek d `elem` [Friday, Saturday] then 2 else 0
    nino  = if esAdulto then 0 else 1

montoReserva :: Map.Map Int Double -> Day -> Day -> [HabitacionOcupada] -> Double
montoReserva tarifas entrada salida habs =
    sum [ fromIntegral (habitacionOcupadaAdultos h) * tarifa (codigoTarifa d True)
        + fromIntegral (habitacionOcupadaNinos h)   * tarifa (codigoTarifa d False)
        | d <- nochesEntre entrada salida, h <- habs ]
  where
    tarifa c = Map.findWithDefault 0 c tarifas

asignarHabitaciones :: Day -> Day -> [Habitacion] -> Map.Map Int Reserva
                    -> [(String, Int, Int)] -> Maybe [HabitacionOcupada]
asignarHabitaciones entrada salida habs reservas = asignar []
  where
    asignar _ [] = Just []
    asignar usadas ((t, a, n) : resto) =
        case filter (disponible usadas t) habs of
            []      -> Nothing
            (h : _) -> (HabitacionOcupada (idHabitacion h) t a n :)
                           <$> asignar (idHabitacion h : usadas) resto
    disponible usadas t h =
        tipoHabitacion h == t
        && idHabitacion h `notElem` usadas
        && libreEnRango entrada salida (idHabitacion h) reservas

mostrarReserva :: Reserva -> IO ()
mostrarReserva r = do
    putStrLn "----------------------------------------"
    putStrLn $ "Reserva #" ++ show (reservaId r)
    putStrLn $ "Nombre:          " ++ reservaNombre r
    putStrLn $ "Fecha y hora:    " ++ reservaFechaHora r
    putStrLn $ "Entrada:         " ++ show (reservaEntrada r)
    putStrLn $ "Salida:          " ++ show (reservaSalida r)
    putStrLn $ "Adultos:         " ++ show (reservaAdulos r)
    putStrLn $ "Niños:           " ++ show (reservaNinos r)
    putStrLn $ "Estado:          " ++ show (reservaEstado r)
    putStrLn $ "Total (sin IVA): " ++ show (reservaSubtotal r)
    putStrLn "Habitaciones:"
    mapM_ mostrarHabitacionOcupada (reservaHabitacionOcupada r)
    putStrLn "----------------------------------------"

mostrarHabitacionOcupada :: HabitacionOcupada -> IO ()
mostrarHabitacionOcupada h =
    putStrLn $ "  ID " ++ show (habitacionOcupadaId h)
            ++ " | " ++ habitacionOcupadaTipo h
            ++ " | adultos: " ++ show (habitacionOcupadaAdultos h)
            ++ " | niños: " ++ show (habitacionOcupadaNinos h)

parsearFecha :: String -> Maybe Day
parsearFecha texto =
    case map readMaybe (separarPor '-' texto) of
        [Just a, Just m, Just d] -> fromGregorianValid a (fromIntegral m) (fromIntegral d)
        _                        -> Nothing

validarSolicitud :: Map.Map String TipoHabitacion -> (String, Int, Int) -> Maybe String
validarSolicitud tipos (t, a, n) =
    case Map.lookup t tipos of
        Nothing -> Just ("El tipo " ++ t ++ " no existe.")
        Just th
            | a < 0 || n < 0 -> Just "Los huéspedes no pueden ser negativos."
            | a + n < 1      -> Just "Cada habitación debe tener al menos un huésped."
            | a + n > cantidadHuespedes th ->
                Just ("El tipo " ++ t ++ " permite máximo "
                      ++ show (cantidadHuespedes th) ++ " huéspedes.")
            | otherwise -> Nothing

crearReserva :: [Habitacion] -> Map.Map String TipoHabitacion -> Map.Map Int Double
             -> Map.Map Int Reserva -> Int -> String -> String
             -> Day -> Day -> Int -> Int -> [(String, Int, Int)]
             -> Either String Reserva
crearReserva habs tipos tarifas reservas nuevoId nombre fechaHora
             entrada salida totalA totalN solicitudes
    | null habs                 = Left "Primero se deben generar las habitaciones del hotel."
    | Map.size tarifas < 8      = Left "Primero se deben cargar las tarifas."
    | null nombre               = Left "El nombre no puede estar vacío."
    | salida <= entrada         = Left "La salida debe ser posterior a la entrada."
    | totalA < 0 || totalN < 0  = Left "Los totales no pueden ser negativos."
    | totalA + totalN < 1       = Left "Se debe reservar al menos una persona."
    | null solicitudes          = Left "Se debe reservar al menos una habitación."
    | (err : _) <- errores      = Left err
    | sumaA /= totalA || sumaN /= totalN =
        Left "La suma de huéspedes por habitación no coincide con los totales."
    | otherwise =
        case asignarHabitaciones entrada salida habs reservas solicitudes of
            Nothing -> Left "No hay disponibilidad para las habitaciones solicitadas en esas fechas."
            Just asignadas -> Right Reserva
                { reservaId = nuevoId
                , reservaNombre = nombre
                , reservaFechaHora = fechaHora
                , reservaEntrada = entrada
                , reservaSalida = salida
                , reservaAdulos = totalA
                , reservaNinos = totalN
                , reservaEstado = Activa
                , reservaHabitacionOcupada = asignadas
                , reservaSubtotal = montoReserva tarifas entrada salida asignadas
                }
  where
    errores = [e | Just e <- map (validarSolicitud tipos) solicitudes]
    sumaA   = sum [a | (_, a, _) <- solicitudes]
    sumaN   = sum [n | (_, _, n) <- solicitudes]
anularReserva :: Int -> Map.Map Int Reserva -> Either String (Map.Map Int Reserva)
anularReserva = cambiarEstado Cancelada

facturarReserva :: Int -> Map.Map Int Reserva -> Either String (Map.Map Int Reserva)
facturarReserva = cambiarEstado Facturada

cambiarEstado :: EstadoReservacion -> Int -> Map.Map Int Reserva -> Either String (Map.Map Int Reserva)
cambiarEstado nuevo idRes reservas =
    case Map.lookup idRes reservas of
        Nothing -> Left "La reservación no existe."
        Just r
            | reservaEstado r /= Activa ->
                Left ("La reservación no está activa (estado: " ++ show (reservaEstado r) ++ ").")
            | otherwise -> Right (Map.insert idRes r { reservaEstado = nuevo } reservas)

habitacionesDisponibles :: Day -> [Habitacion] -> Map.Map Int Reserva -> [Habitacion]
habitacionesDisponibles d habs reservas =
    filter (\h -> ocupadaEn d (idHabitacion h) reservas == Nothing) habs

impuesto :: Double -> Double
impuesto subtotal = subtotal * 0.13

mostrarFactura :: Int -> InfoHotel -> Reserva -> IO ()
mostrarFactura idFactura hotel r = do
    let subtotal = reservaSubtotal r
        iva      = impuesto subtotal
    putStrLn "========================================"
    putStrLn $ "FACTURA #" ++ show idFactura
    putStrLn $ "Reserva #" ++ show (reservaId r)
    putStrLn "----------------------------------------"
    putStrLn $ hotelNombre hotel
    putStrLn $ "Cédula jurídica: " ++ hotelCedulaJuridica hotel
    putStrLn $ "Sitio web:       " ++ hotelSitioWeb hotel
    putStrLn $ "Teléfono:        " ++ hotelTelefono hotel
    putStrLn $ "Ubicación:       " ++ hotelProvincia hotel ++ ", " ++ hotelPais hotel
    putStrLn "----------------------------------------"
    putStrLn $ "Subtotal:        " ++ show subtotal
    putStrLn $ "IVA (13%):       " ++ show iva
    putStrLn $ "Total:           " ++ show (subtotal + iva)
    putStrLn "\nDetalle de la reservación:"
    mostrarReserva r