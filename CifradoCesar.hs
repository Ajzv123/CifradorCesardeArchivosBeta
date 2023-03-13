--Cifrado de cesar en español
import Data.Char
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

-- Declaración de la lista que contiene el alfabeto
abc = ['a'..'n'] ++ ['ñ'] ++ ['o'..'z']
-- Declaración de la lista que contiene el alfabeto en mayúsculas
abc' = ['A'..'N'] ++ ['Ñ'] ++ ['O'..'Z']


-- Función que devuelve la posición de un caracter en el alfabeto
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i ) <- zip xs [0..n], x == x']
 where 
   n = length xs-1

-- Función que devuelve la posición de un caracter en el alfabeto en mayúsculas
positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = [i | (x', i ) <- zip xs [0..n], x == x']
 where 
   n = length xs-1

-- Función que devuelve la cantidad de letras minúsculas que hay en una cadena
lowers :: String -> Int
lowers xs = length [x |x <- xs, isLower x ]

-- Función que devuelve la cantidad de letras mayúsculas que hay en una cadena
uppers :: String -> Int
uppers xs = length [x | x <- xs, isUpper x ]

-- Función que devuelve la cantidad de veces que aparece un caracter en una cadena
count :: Char -> String -> Int
count x xs = length [x' |x' <- xs, x==x']

-- Función que devuelve la posición de un caracter en el alfabeto usando nuestra lista de posiciones
let2int :: Char -> Int
let2int c = head (positions c abc)

-- Función que devuelve la posición de un caracter en el alfabeto en mayúsculas usando nuestra lista de posiciones
let2int' :: Char -> Int
let2int' c = head (positions' c abc')

-- Función que devuelve el caracter que se encuentra en la posición n del alfabeto
int2let :: Int -> Char
int2let n = abc !! n

-- Función que devuelve el caracter que se encuentra en la posición n del alfabeto en mayúsculas
int2let' :: Int -> Char
int2let' n = abc' !! n

--Función shift que aplica un desplazamiento a un caracter
shift :: Int -> Char -> Char
shift n c 
   | isLower c = int2let ((let2int c + n) `mod` 27)
   | isUpper c = int2let' ((let2int' c + n) `mod` 27)
   | otherwise = c

-- Función que codifica una cadena usando un desplazamiento n
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs ]

--Tabla de frecuencias de las letras del alfabeto en español
table :: [Float]
table = [12.53, 1.42, 4.68, 5.86, 13.68, 0.69, 1.01, 0.70, 6.25, 0.44, 0.02, 4.97, 3.15, 6.71, 0.31, 8.68, 2.51, 0.88, 6.87, 7.98, 4.62, 3.93, 0.90, 0.01, 0.22, 0.90, 0.52]

-- Función que devuelve la frecuencia de un caracter en una cadena
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- Función que devuelve la frecuencia de cada letra del alfabeto en una cadena
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
 where 
   n = lowers xs

--Funcion que devuelve la frecuencia de cada letra del alfabeto en una cadena en mayúsculas
freqs' :: String -> [Float]
freqs' xs = [percent (count x xs) n | x <- ['A'..'Z']]
 where 
   n = uppers xs

--función que devuelve la posición de la letra que más se repite en una cadena
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o,e) <- zip os es]

-- Función que devuelve el desplazamiento que se debe aplicar para descifrar un mensaje
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

--Función para crackear un mensaje cifrado con el cifrado de cesar que acepte mayúsculas y minúsculas
crack :: String -> String
crack xs = encode (-factor) xs
 where 
   factor = head (positions (minimum chitab) chitab)
   chitab = [chisqr (rotate n table') table | n <- [0..27]]
   table' = if uppers xs == 0 then freqs xs else if lowers xs == 0 then freqs' xs else freqs xs ++ freqs' xs


-- crack que saca el caracter ñ como letra y no como numerico
crack' xs = putStrLn $ crack  xs

-- Encode que saca el caracter ñ como letra y no como numerico
encode' n xs = putStrLn $ encode n xs 
-- encode (-13) "Tqotb cbe NVMI123 (Nxpb Vbqx Fnzotqm \209qzugqm)" Ponerlo en el codificador


--Función que va a encriptar un archivo .hs y lo va a guardar en un archivo .hs
encriptar :: IO ()
encriptar = do
    putStrLn "Ingrese la ruta del archivo a encriptar"
    nombre <- getLine
    --Mostrar el archivo a encriptar
    showFile <- readFile nombre    
    putStrLn "Ingresa la ruta del archivo encriptado"
    nombre2 <- getLine
    putStrLn "Ingrese el desplazamiento"
    desplazamiento <- getLine
    contenido <- readFile nombre
    writeFile nombre2 (encode (read desplazamiento) contenido)
    putStrLn "Archivo encriptado"
{-
Esta función trabaja como deberia, pero no puede encriptar archivos con una gran cantidad de texto
es por eso que se creo la función encriptar' que usa TIO, que igual tiene sus desventajas
-}

--Función que va a encriptar un archivo y lo va a guardar en un archivo, usando TIO
encriptar' :: IO ()
encriptar' = do
      putStrLn "Ingrese la ruta del archivo a encriptar"
      nombre <- getLine
      putStrLn "Ingresa la ruta del archivo encriptado"
      nombre2 <- getLine
      putStrLn "Ingrese el desplazamiento"
      desplazamiento <- getLine
      contenido <- TIO.readFile nombre
      let contenidoEncriptado = encode (read desplazamiento) (T.unpack contenido)
      putStrLn contenidoEncriptado
      TIO.writeFile nombre2 (T.pack contenidoEncriptado)
      putStrLn "Archivo encriptado"
{-
Por lo visto ya se pueden encriptar documentos mas grandes pero con una desventaja,
si bien el cifrador puede trabajar con ñ, este metodo no puede entenderlas, por lo que
teorizo no puede leerlas debido a la funcion TIO o T alguna de ellas o las dos no puede leer dicho caracter
-}

{-
El programa esta en una versión muy temprana, aun se debe trabajar en que acepte caracteres especiales o que los ignore
y que pueda encriptar archivos con una gran cantidad de texto que contengan ñ o algún otro caracter especial
-}