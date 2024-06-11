esLetra :: Char -> Bool
esLetra caracter = (caracter >= 'a' && caracter <= 'z') || (caracter >= 'A' && caracter <= 'Z') || caracter == 'ñ' || caracter == 'Ñ'

main :: IO()
main = do
    print("Evalurar numero 5")
    print(esLetra('5'))
    print("Evalurar letra a")
    print(esLetra('a'))
    print("Evalurar letra A")
    print(esLetra('A'))
    print("Evalurar simbolo %")
    print(esLetra('%'))
    print("Evalurar letra ñ")
    print(esLetra('ñ'))
