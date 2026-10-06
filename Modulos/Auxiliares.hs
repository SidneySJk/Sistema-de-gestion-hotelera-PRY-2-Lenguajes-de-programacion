-- Auxiliares que remplazan el uso de librerias externas

module Modulos.Auxiliares where
import Data.List    
separarPor separador cadena = do
    let lista = filter (/= separador) cadena
    return lista