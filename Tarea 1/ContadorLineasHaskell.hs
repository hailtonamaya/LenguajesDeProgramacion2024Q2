import System.IO
import System.IO.Error (catchIOError)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf)
import Text.Regex.Posix ((=~))

-- Funcion que determina si una linea es de codigo
isLineOfCode :: String -> Bool
isLineOfCode line =
    let
        -- Remover string con doble comillas
        line' = line =~ "\".*?\"" :: String
        -- Remover string con comillas simples
        line'' = line' =~ "'.*?'" :: String
        -- Remove comentarios de una linea
        line''' = line'' =~ "//.*" :: String
        -- Remover bloques
        line'''' = removeInlineBlockComments line'''
    in
        not (all isSpace line'''') && not (null line'''') 

-- Funcion que remueve comentarios de bloque en la misma linea
removeInlineBlockComments :: String -> String
removeInlineBlockComments line =
    if "/*" `isInfixOf` line && "*/" `isInfixOf` line
    then
        let
            beforeComment = takeWhile (/= '/') line
            afterComment = drop (length beforeComment + 2) $ dropWhile (/= '*') line
            restLine = dropWhile (/= '/') $ drop 1 $ dropWhile (/= '*') afterComment
        in beforeComment ++ removeInlineBlockComments restLine
    else line

-- Funcion que cuenta las lineas de codigo
countLinesOfCode filePath = do
    contents <- readFile filePath `catchIOError` \_ -> do
        putStrLn "Error al abrir el archivo"
        return ""
    let linesOfFile = lines contents
    return $ countLines linesOfFile False 0

    
countLines :: [String] -> Bool -> Int -> Int
countLines [] _ count = count
countLines (line:rest) inBlockComment count =
    let
        strippedLine = dropWhile isSpace line
        (newBlockComment, strippedLine') = handleBlockComment inBlockComment strippedLine
    in
        if newBlockComment
        then countLines rest newBlockComment count
        else if isLineOfCode strippedLine'
             then countLines rest newBlockComment (count + 1)
             else countLines rest newBlockComment count

-- Manejar comentarios de bloque
handleBlockComment :: Bool -> String -> (Bool, String)
handleBlockComment inBlockComment line
    | inBlockComment =
        if "*/" `isInfixOf` line
        then (False, drop 2 $ dropWhile (/= '/') $ drop 1 $ dropWhile (/= '*') line)
        else (True, "")
    | "/*" `isInfixOf` line =
        if "*/" `isInfixOf` line
        then (False, takeWhile (/= '/') line ++ drop 2 (dropWhile (/= '/') $ drop 1 $ dropWhile (/= '*') line))
        else (True, takeWhile (/= '/') line)
    | otherwise = (False, line)

-- Main 
main :: IO ()
main = do
    let filePath = "archivo.java"
    linesOfCode <- countLinesOfCode filePath
    putStrLn $ "Numero de lineas de codigo: " ++ show linesOfCode
