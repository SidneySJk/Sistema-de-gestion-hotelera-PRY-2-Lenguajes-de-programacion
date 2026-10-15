-- Auxiliares que remplazan el uso de librerias externas

module Modulos.Auxiliares where
import Text.Printf (printf)
separarPor :: Char -> String -> [String]
separarPor sep cadena = case break (== sep) cadena of
    (antes, [])      -> [antes]
    (antes, _:resto) -> antes : separarPor sep resto

formatearMonto :: Double -> String
formatearMonto = printf "%.2f"