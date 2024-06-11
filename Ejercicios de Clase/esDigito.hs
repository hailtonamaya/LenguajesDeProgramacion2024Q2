esDigito :: Char -> Bool
esDigito caracter = caracter >= '0' && caracter <= '9'

main :: IO();
main = do
    print(esDigito('5'))

