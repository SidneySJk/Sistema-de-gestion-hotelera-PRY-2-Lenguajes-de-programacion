module Modulos.Sistema where
import Modulos.Habitaciones
import Modulos.Info
import Modulos.Reservacion
import Modulos.Auxiliares (separarPor)
import qualified Data.Map as Map
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Exception (try, IOException)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

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

carpetaDatos :: FilePath
carpetaDatos = "data/"

rutaDatos :: String -> FilePath
rutaDatos nombre = carpetaDatos ++ nombre

archivoHotel :: FilePath
archivoHotel = rutaDatos "hotel.txt"

archivoSistema :: FilePath
archivoSistema = rutaDatos "sistema.txt"

iniciar :: IO ()
iniciar = do
    guardado <- cargarSistema
    resultado <- leerSeguro (cargarInfoHotel archivoHotel)
    hotel <- case resultado of
        Just (Just h) -> return (Just h)
        _ -> do
            putStrLn ("Aviso: no se pudo cargar la informacion del hotel desde " ++ archivoHotel ++ ".")
            return (sistemaHotel guardado)
    menuPrincipal guardado { sistemaHotel = hotel }

-- ============================================================
-- Persistencia
-- ============================================================

leerArchivoCompleto :: FilePath -> IO String
leerArchivoCompleto ruta = do
    contenido <- readFile ruta
    length contenido `seq` return contenido

cargarSistema :: IO Sistema
cargarSistema = do
    resultado <- leerSeguro (leerArchivoCompleto archivoSistema)
    case resultado of
        Nothing -> return sistemaInicial
        Just contenido ->
            case readMaybe contenido of
                Just s  -> do
                    putStrLn ("Datos cargados desde " ++ archivoSistema ++ ".")
                    return s
                Nothing -> do
                    putStrLn ("Aviso: " ++ archivoSistema ++ " esta dañado, se inicia vacio.")
                    return sistemaInicial

guardarSistema :: Sistema -> IO Sistema
guardarSistema s = do
    resultado <- leerSeguro (writeFile archivoSistema (show s))
    case resultado of
        Nothing -> putStrLn ("Aviso: no se pudieron guardar los datos en " ++ archivoSistema ++ ".")
        Just () -> return ()
    return s

-- ============================================================
-- Entrada de datos
-- ============================================================

leerSeguro :: IO a -> IO (Maybe a)
leerSeguro accion = do
    r <- try accion
    case r of
        Left e  -> const (return Nothing) (e :: IOException)
        Right x -> return (Just x)

preguntar :: String -> IO String
preguntar msg = do
    putStr msg
    hFlush stdout
    filter (/= '\r') <$> getLine

pedirFecha :: String -> IO Day
pedirFecha msg = do
    txt <- preguntar msg
    case parsearFecha txt of
        Just d  -> return d
        Nothing -> putStrLn "Fecha invalida, use el formato AAAA-MM-DD." >> pedirFecha msg

pedirEntero :: String -> (Int -> Bool) -> String -> IO Int
pedirEntero msg cond err = do
    txt <- preguntar msg
    case readMaybe txt of
        Just n | cond n -> return n
        _               -> putStrLn err >> pedirEntero msg cond err

-- ============================================================
-- Menu principal
-- ============================================================

menuPrincipal :: Sistema -> IO ()
menuPrincipal s = do
    putStrLn "\n============================"
    putStrLn "  Sistema gestor de hoteles"
    putStrLn "============================"
    putStrLn "1. Menu Administrativo"
    putStrLn "2. Menu General"
    putStrLn "3. Salir"
    op <- preguntar "Ingrese su opcion: "
    case op of
        "1" -> menuAdmin s   >>= menuPrincipal
        "2" -> menuGeneral s >>= menuPrincipal
        "3" -> putStrLn "Adios."
        _   -> putStrLn "Opcion invalida." >> menuPrincipal s

-- ============================================================
-- Menu administrativo
-- ============================================================

menuAdmin :: Sistema -> IO Sistema
menuAdmin s = do
    putStrLn "\n------ Menu Administrativo ------"
    putStrLn "1. Cargar tipos de habitacion"
    putStrLn "2. Asignar cantidad de habitaciones por tipo"
    putStrLn "3. Ver habitaciones"
    putStrLn "4. Cargar tarifas"
    putStrLn "5. Consultar reservaciones"
    putStrLn "6. Consultar estado de ocupacion"
    putStrLn "7. Estadisticas de ocupacion"
    putStrLn "8. Volver"
    op <- preguntar "Ingrese su opcion: "
    case op of
        "1" -> opcionCargarTipos s >>= guardarSistema >>= menuAdmin
        "2" -> opcionAsignarHabitaciones s >>= guardarSistema >>= menuAdmin
        "3" -> opcionVerHabitaciones s >> menuAdmin s
        "4" -> opcionCargarTarifas s >>= guardarSistema >>= menuAdmin
        "5" -> opcionConsultarReservas s >> menuAdmin s
        "6" -> opcionEstadoOcupacion s >> menuAdmin s
        "7" -> menuEstadisticas s >> menuAdmin s
        "8" -> return s
        _   -> putStrLn "Opcion invalida." >> menuAdmin s

opcionCargarTipos :: Sistema -> IO Sistema
opcionCargarTipos s = do
    nombre <- preguntar "Nombre del archivo de tipos (en data/): "
    resultado <- leerSeguro (listaTiposHabitaciones (rutaDatos nombre))
    case resultado of
        Nothing -> putStrLn "No se pudo leer el archivo." >> return s
        Just nuevos -> do
            let tiposActuales = sistemaTipos s
                agregar m t = Map.insertWith (\_ viejo -> viejo) (tipo t) t m
                tiposFinales = foldl agregar tiposActuales nuevos
                agregados = Map.size tiposFinales - Map.size tiposActuales
            putStrLn ("Tipos agregados: " ++ show agregados
                      ++ ". Total de tipos: " ++ show (Map.size tiposFinales))
            mapM_ (\t -> putStrLn ("  " ++ tipo t ++ " - " ++ descripcion t
                                   ++ " (max. " ++ show (cantidadHuespedes t) ++ ")"))
                  (Map.elems tiposFinales)
            return s { sistemaTipos = tiposFinales }

opcionAsignarHabitaciones :: Sistema -> IO Sistema
opcionAsignarHabitaciones s
    | not (null (sistemaHabitaciones s)) =
        putStrLn "Las habitaciones ya fueron generadas, no se puede hacer de nuevo." >> return s
    | Map.null (sistemaTipos s) =
        putStrLn "Primero debe cargar los tipos de habitacion." >> return s
    | otherwise = do
        habs <- generar 1 (Map.elems (sistemaTipos s))
        if null habs
            then putStrLn "No se genero ninguna habitacion." >> return s
            else do
                putStrLn ("Se generaron " ++ show (length habs) ++ " habitaciones.")
                return s { sistemaHabitaciones = habs }
  where
    generar _ [] = return []
    generar sigId (t:ts) = do
        n <- pedirEntero ("Cantidad de habitaciones " ++ tipo t ++ ": ")
                         (>= 0) "Debe ser un numero mayor o igual a 0."
        resto <- generar (sigId + n) ts
        return (crearHabitaciones sigId t n ++ resto)

opcionVerHabitaciones :: Sistema -> IO ()
opcionVerHabitaciones s
    | null (sistemaHabitaciones s) = putStrLn "No hay habitaciones generadas."
    | otherwise = mostrarHabitaciones (sistemaHabitaciones s)

parsearTarifa :: String -> Maybe (Int, Double)
parsearTarifa linea =
    case separarPor ',' linea of
        [c, m] -> do
            codigo <- readMaybe c
            monto  <- readMaybe m
            if codigo >= 1 && codigo <= 8 && monto >= 0 then Just (codigo, monto) else Nothing
        _ -> Nothing

opcionCargarTarifas :: Sistema -> IO Sistema
opcionCargarTarifas s = do
    nombre <- preguntar "Nombre del archivo de tarifas (en data/): "
    resultado <- leerSeguro (readFile (rutaDatos nombre))
    case resultado of
        Nothing -> putStrLn "No se pudo leer el archivo." >> return s
        Just contenido -> do
            let lineas = filter (not . null) (map (filter (/= '\r')) (lines contenido))
                tarifas = Map.fromList [t | Just t <- map parsearTarifa lineas]
            if Map.size tarifas /= 8
                then do
                    putStrLn "El archivo debe tener las 8 tarifas (codigos 1 al 8) con formato codigo,monto."
                    return s
                else do
                    putStrLn "Tarifas cargadas:"
                    mapM_ (\(c, m) -> putStrLn ("  " ++ show c ++ ": " ++ show m)) (Map.toList tarifas)
                    return s { tarifasSistema = tarifas }

opcionConsultarReservas :: Sistema -> IO ()
opcionConsultarReservas s
    | Map.null (reservasSistema s) = putStrLn "No hay reservaciones registradas."
    | otherwise = mapM_ mostrarReserva (Map.elems (reservasSistema s))

opcionEstadoOcupacion :: Sistema -> IO ()
opcionEstadoOcupacion s
    | null (sistemaHabitaciones s) = putStrLn "No hay habitaciones generadas."
    | otherwise = do
        d <- pedirFecha "Dia a consultar (AAAA-MM-DD): "
        let reservas = reservasSistema s
            estado h = (h, ocupadaEn d (idHabitacion h) reservas)
            estados = map estado (sistemaHabitaciones s)
        putStrLn "\nDisponibles:"
        mapM_ (\(h, _) -> putStrLn ("  ID " ++ show (idHabitacion h) ++ " | " ++ tipoHabitacion h))
              [e | e@(_, Nothing) <- estados]
        putStrLn "Ocupadas:"
        mapM_ (\(h, r) -> putStrLn ("  ID " ++ show (idHabitacion h) ++ " | " ++ tipoHabitacion h
                                    ++ " | reserva #" ++ show r))
              [(h, r) | (h, Just r) <- estados]

menuEstadisticas :: Sistema -> IO ()
menuEstadisticas s = do
    putStrLn "\n------ Estadisticas ------"
    putStrLn "1. Total de huespedes por mes-año"
    putStrLn "2. Habitaciones ocupadas en un dia"
    putStrLn "3. Total de habitaciones no ocupadas en un dia"
    putStrLn "4. Monto recaudado con impuestos"
    putStrLn "5. Top 3 dias de la semana con mas ocupacion"
    putStrLn "6. Volver"
    op <- preguntar "Ingrese su opcion: "
    case op of
        "6" -> return ()
        _ | op `elem` ["1", "2", "3", "4", "5"] ->
                putStrLn "(estadistica pendiente)" >> menuEstadisticas s
          | otherwise -> putStrLn "Opcion invalida." >> menuEstadisticas s

-- ============================================================
-- Menu general
-- ============================================================

menuGeneral :: Sistema -> IO Sistema
menuGeneral s = do
    putStrLn "\n------ Menu General ------"
    putStrLn "1. Ver disponibilidad"
    putStrLn "2. Reservacion"
    putStrLn "3. Anular reservacion"
    putStrLn "4. Facturar reservacion"
    putStrLn "5. Volver"
    op <- preguntar "Ingrese su opcion: "
    case op of
        "1" -> opcionDisponibilidad s >> menuGeneral s
        "2" -> opcionReservar s >>= guardarSistema >>= menuGeneral
        "3" -> opcionAnular s >>= guardarSistema >>= menuGeneral
        "4" -> opcionFacturar s >>= guardarSistema >>= menuGeneral
        "5" -> return s
        _   -> putStrLn "Opcion invalida." >> menuGeneral s

opcionDisponibilidad :: Sistema -> IO ()
opcionDisponibilidad s
    | null (sistemaHabitaciones s) = putStrLn "No hay habitaciones generadas."
    | otherwise = do
        d <- pedirFecha "Dia a consultar (AAAA-MM-DD): "
        case habitacionesDisponibles d (sistemaHabitaciones s) (reservasSistema s) of
            []    -> putStrLn "No hay habitaciones disponibles ese dia."
            habs  -> do
                putStrLn ("Habitaciones disponibles el " ++ show d ++ ":")
                mapM_ (\h -> putStrLn ("  ID " ++ show (idHabitacion h) ++ " | " ++ tipoHabitacion h)) habs

opcionReservar :: Sistema -> IO Sistema
opcionReservar s
    | null (sistemaHabitaciones s) = putStrLn "Primero se deben generar las habitaciones del hotel." >> return s
    | Map.size (tarifasSistema s) < 8 = putStrLn "Primero se deben cargar las tarifas." >> return s
    | otherwise = do
        putStrLn "\n=== Nueva reservacion ==="
        nombre  <- preguntar "Nombre de quien reserva: "
        entrada <- pedirFecha "Fecha de entrada (AAAA-MM-DD): "
        salida  <- pedirFecha "Fecha de salida (AAAA-MM-DD): "
        totalA  <- pedirEntero "Total de adultos: " (>= 0) "Debe ser un numero mayor o igual a 0."
        totalN  <- pedirEntero "Total de niños: " (>= 0) "Debe ser un numero mayor o igual a 0."
        cant    <- pedirEntero "Cantidad de habitaciones: " (>= 1) "Debe ser al menos 1."
        putStrLn ("Tipos disponibles: " ++ unwords (Map.keys (sistemaTipos s)))
        solicitudes <- mapM pedirSolicitud [1 .. cant]
        ahora <- getZonedTime
        let fechaHora = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" ahora
        case crearReserva (sistemaHabitaciones s) (sistemaTipos s) (tarifasSistema s)
                          (reservasSistema s) (sigIdReserva s) nombre fechaHora
                          entrada salida totalA totalN solicitudes of
            Left err -> putStrLn ("No se pudo reservar: " ++ err) >> return s
            Right r  -> do
                putStrLn "\nReservacion realizada. Comprobante:"
                mostrarReserva r
                return s { reservasSistema = Map.insert (reservaId r) r (reservasSistema s)
                         , sigIdReserva = sigIdReserva s + 1 }
  where
    pedirSolicitud i = do
        putStrLn ("\nHabitacion " ++ show i)
        t <- preguntar "Tipo: "
        a <- pedirEntero "Adultos: " (>= 0) "Debe ser un numero mayor o igual a 0."
        n <- pedirEntero "Niños: " (>= 0) "Debe ser un numero mayor o igual a 0."
        return (t, a, n)

opcionAnular :: Sistema -> IO Sistema
opcionAnular s = do
    idRes <- pedirEntero "Codigo de la reservacion: " (const True) "Debe ser un numero."
    case anularReserva idRes (reservasSistema s) of
        Left err -> putStrLn err >> return s
        Right reservas -> do
            putStrLn ("La reservacion #" ++ show idRes ++ " fue anulada.")
            return s { reservasSistema = reservas }

opcionFacturar :: Sistema -> IO Sistema
opcionFacturar s =
    case sistemaHotel s of
        Nothing -> putStrLn "Falta cargar la informacion del hotel." >> return s
        Just hotel -> do
            idRes <- pedirEntero "Codigo de la reservacion: " (const True) "Debe ser un numero."
            case facturarReserva idRes (reservasSistema s) of
                Left err -> putStrLn err >> return s
                Right reservas -> do
                    mostrarFactura (sigIdFactura s) hotel (reservas Map.! idRes)
                    return s { reservasSistema = reservas, sigIdFactura = sigIdFactura s + 1 }