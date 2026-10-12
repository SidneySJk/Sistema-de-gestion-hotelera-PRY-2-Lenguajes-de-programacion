-- Auxiliares que remplazan el uso de librerias externas

module Modulos.Auxiliares where
import Data.List    
separarPor :: Char -> String -> [String]
separarPor sep cadena = case break (== sep) cadena of
    (antes, [])      -> [antes]
    (antes, _:resto) -> antes : separarPor sep resto

