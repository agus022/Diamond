--Creamos la funcion que toma un carácter como entrada y regresa un diamante representado como una lista de cadenas de caracteres.
--Si el carácter de entrada no está en el rango de 'A' a 'Z', devuelve 'VACIO' indicando una entrada inválida.
--la función construye la parte superior del diamante utilizando el carácter de entrada y luego refleja esta parte superior para construir la parte inferior.
--utiliza 'makeRow' para generar cada fila del diamante.
diamond :: Char--Carácter para construir el diamante
        -> Maybe [String]--El diamante como una lista de cadenas de caracteres, o 'VACIO' si la entrada es inválida
diamond c
    | c `notElem` ['A'..'Z'] = Nothing--Validar la entrada
    | otherwise = Just $ topRows ++ tail (reverse topRows)
    where
        topRows = [makeRow n c | n <- ['A'..c]]
--La función 'makeRow' genera una fila del diamante a partir de un carácter dado y el carácter máximo del diamante.
--Toma un carácter y el carácter máximo del diamante como entrada y devuelve una cadena de caracteres que representa una fila del diamante.
--   La cantidad de puntos en los espacios vacíos depende de la posición del carácter dentro del diamante y del carácter máximo del diamante.
makeRow :: Char  --Carácter para construir la fila
        -> Char  --Carácter máximo del diamante
        -> String --fila del diamante
makeRow 'A' maxChar = let spaces = replicate (ord maxChar - ord 'A') '.'  --puntos en los espacios vacíos.
                          middleSpaces = replicate (2 * (ord 'A' - ord 'A') - 1) '.' --puntos en los espacios vacíos.
                      in spaces ++ "A" ++ middleSpaces ++ spaces
makeRow c maxChar = let spaces = replicate (ord maxChar - ord c) '.' --puntos en los espacios vacíos.
                        middleSpaces = replicate (2 * (ord c - ord 'A') - 1) '.'--puntos en los espacios vacíos.
                    in spaces ++ [c] ++ middleSpaces ++ [c] ++ spaces

--función 'ord' devuelve el valor ordinal de un carácter en relación con 'A'.
--Toma un carácter como entrada y devuelve su valor ordinal en relación con 'A'.
--para el carácter 'A' devuelve 0, para 'B' devuelve 1, y así sucesivamente.
ord :: Char --carácter para calcular el valor ordinal
    -> Int  --valor ordinal del carácter en relación con 'A'
ord c = fromEnum c - fromEnum 'A'

--Función principal 'main' que prueba la función 'diamond' con un ejemplo.
main :: IO ()
main = case diamond 'I' of  --definir el valor maximo del diamante 
    Just lines -> putStrLn $ unlines lines
    Nothing -> putStrLn "Entrada inválida"

---JAFS

