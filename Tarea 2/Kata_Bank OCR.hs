import Data.List
import Data.Maybe (mapMaybe)

encriptadoaNumero :: [String] -> Maybe Char
encriptadoaNumero [" _ ", "| |", "|_|"] = Just '0'
encriptadoaNumero ["   ", "  |", "  |"] = Just '1'
encriptadoaNumero [" _ ", " _|", "|_ "] = Just '2'
encriptadoaNumero [" _ ", " _|", " _|"] = Just '3'
encriptadoaNumero ["   ", "|_|", "  |"] = Just '4'
encriptadoaNumero [" _ ", "|_ ", " _|"] = Just '5'
encriptadoaNumero [" _ ", "|_ ", "|_|"] = Just '6'
encriptadoaNumero [" _ ", "  |", "  |"] = Just '7'
encriptadoaNumero [" _ ", "|_|", "|_|"] = Just '8'
encriptadoaNumero [" _ ", "|_|", " _|"] = Just '9'
encriptadoaNumero _                    = Nothing

grupos4Lineas :: [String] -> [[String]]
grupos4Lineas [] = []
grupos4Lineas lista = take 4 lista : grupos4Lineas (drop 4 lista)

dividirEn3 :: String -> [String]
dividirEn3 [] = []
dividirEn3 lista = take 3 lista : dividirEn3 (drop 3 lista)

convLineasAEncri :: [String] -> [[String]]
convLineasAEncri [a, b, c] = transpose [dividirEn3 a, dividirEn3 b, dividirEn3 c]
convLineasAEncri _ = []

encriptadoaN :: [[String]] -> [Char]
encriptadoaN = mapMaybe encriptadoaNumero

main :: IO ()
main = do
    contents <- readFile "archivo_prueba.txt"
    let groupedLines = grupos4Lineas (lines contents)
    let segments = map (convLineasAEncri . take 3) groupedLines
    let digits = map encriptadoaN segments
    mapM_ putStrLn digits
