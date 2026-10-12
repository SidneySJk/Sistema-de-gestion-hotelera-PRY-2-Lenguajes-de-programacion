module Modulos.Info where

data InfoHotel = InfoHotel{ 
    hotelNombre, 
    hotelCedulaJuridica, 
    hotelSitioWeb, 
    hotelTelefono, 
    hotelPais, 
    hotelProvincia :: String
  } deriving (Show, Read, Eq)

mainInfo :: IO()
mainInfo = do
    putStrLn "info"