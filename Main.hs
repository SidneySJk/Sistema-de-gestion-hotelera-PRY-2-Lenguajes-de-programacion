module Main where
import Modulos.Sistema
import System.IO

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8
    iniciar