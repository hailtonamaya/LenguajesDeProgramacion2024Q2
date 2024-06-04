import System.IO
import Data.List

-- Definimos la representación gráfica de cada dígito en una lista de cadenas
digitToSegments :: Char -> [String]
digitToSegments '0' = [" _ ", "| |", "|_|"]
digitToSegments '1' = ["   ", "  |", "  |"]
digitToSegments '2' = [" _ ", " _|", "|_ "]
digitToSegments '3' = [" _ ", " _|", " _|"]
digitToSegments '4' = ["   ", "|_|", "  |"]
digitToSegments '5' = [" _ ", "|_ ", " _|"]
digitToSegments '6' = [" _ ", "|_ ", "|_|"]
digitToSegments '7' = [" _ ", "  |", "  |"]
digitToSegments '8' = [" _ ", "|_|", "|_|"]
digitToSegments '9' = [" _ ", "|_|", " _|"]
digitToSegments _   = ["   ", "   ", "   "]  -- Espacio para caracteres no reconocidos

-- Función para convertir una cadena de dígitos en su representación gráfica
convertDigits :: String -> [String]
convertDigits digits = map concat $ transpose $ map digitToSegments digits

-- Función principal que lee el archivo, procesa los dígitos y los imprime
main :: IO ()
main = do
    -- Leer el archivo de entrada
    contents <- readFile "input.txt"
    -- Procesar cada línea del archivo
    let linesOfDigits = lines contents
    let outputLines = concatMap convertDigits linesOfDigits
    -- Imprimir la salida
    mapM_ putStrLn outputLines
