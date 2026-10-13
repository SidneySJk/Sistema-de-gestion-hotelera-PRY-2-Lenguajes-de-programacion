module Modulos.Info where
import Modulos.Auxiliares (separarPor)

data InfoHotel = InfoHotel{
    hotelNombre,
    hotelCedulaJuridica,
    hotelSitioWeb,
    hotelTelefono,
    hotelPais,
    hotelProvincia :: String
  } deriving (Show, Read, Eq)

parsearInfoHotel :: String -> Maybe InfoHotel
parsearInfoHotel linea =
    case separarPor ',' (filter (/= '\r') linea) of
        [n, c, w, t, p, pr] -> Just (InfoHotel n c w t p pr)
        _                   -> Nothing

cargarInfoHotel :: FilePath -> IO (Maybe InfoHotel)
cargarInfoHotel ruta = do
    contenido <- readFile ruta
    case lines contenido of
        (l:_) -> return (parsearInfoHotel l)
        []    -> return Nothing