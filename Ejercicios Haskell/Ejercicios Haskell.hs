
esDigito :: Char -> Bool
esDigito caracter = caracter >= '0' && caracter <= '9'

contadorNumero :: String -> Int
contadorNumero[] = 0 
contadorNumero (x:xs) = if esDigito x then 1 + contadorNumero xs else contadorNumero xs

invertirCadena :: String -> String
invertirCadena[] = []
invertirCadena (x:xs) = invertirCadena xs ++ [x]

devolverDigitos :: String -> String
devolverDigitos[] = []
devolverDigitos (x:xs) = if esDigito x then [x] ++ devolverDigitos xs else devolverDigitos xs

eliminarUlt :: String  -> String
eliminarUlt[] = []
eliminarUlt (x:xs) = if xs == [] then [] else [x] ++ eliminarUlt xs

main :: IO()
main = do
    print("----Contador Numero----")
    print(contadorNumero "1234a5b6d")
    print(contadorNumero "abc2")
    print(contadorNumero "abc")

    print("----Invertir Cadena----")
    print(invertirCadena "abcd")
    print(invertirCadena "aeiou")
    print(invertirCadena "1234")

    print("----Devolver Digitos----")
    print(devolverDigitos "abc123")
    print(devolverDigitos "abc")
    print(devolverDigitos "123")

    print("----Eliminar Ultimo----")
    print(eliminarUlt "colas")
    print(eliminarUlt "hola como estas")
    print(eliminarUlt "1234")