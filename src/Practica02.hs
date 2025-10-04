module Practica02 where

--BINARIOS
data Bit = O | I 
        deriving (Show, Eq)

type Binario = [Bit]

--BINARIOS

-- Convierte un bit a un número entero
bitToInt :: Bit -> Int
bitToInt O = 0
bitToInt I = 1

-- Convierte un número entero (0 o 1) a bit
intToBit :: Int -> Bit
intToBit 0 = O
intToBit 1 = I

-- Convierte un binario (lista de bits) a su valor decimal
toDecimal :: Binario -> Int
toDecimal [] = 0
toDecimal (x:xs) = (bitToInt x * 2^(myLength xs)) + toDecimal xs

-- Convierte un número decimal a binario
toBin :: Int -> Binario
toBin 0 = [O]
toBin 1 = [I]
toBin x = toBin (x `div` 2) ++ [intToBit (x `mod` 2)]

-- Suma dos binarios convirtiéndolos a decimal y luego de vuelta a binario
suma :: Binario -> Binario -> Binario
suma [] [] = []
suma [] y = y
suma x [] = x
suma x y = toBin ( toDecimal x + toDecimal y)

--LISTAS

-- Verifica si una lista es palíndromo (igual al reverso)
palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == myReverse xs 

--Funcion principal que calcula la diferencia simetrica (Aquellos elementos que esten en la union pero no en la interseccion)
diferenciaSimetrica :: Eq a => [a] -> [a] -> [a]
diferenciaSimetrica xs ys = diferencia xs ys ++ diferencia ys xs

--Conjunto potencia - Calcula el conjunto potencia de una lista (todas las combinaciones posibles)
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = [(x:ys) | ys <- conjuntoPotencia xs] ++ conjuntoPotencia xs


--LISTAS DE LONGITUD PAR

--Esta va de regalo
type ListaPar a b = [(a,b)]

--Longitud - Calcula la longitud de una lista de pares (cada par cuenta como 2)
longitud :: ListaPar a b -> Int
longitud [] = 0
longitud [x] = 2
longitud (x:xs) = 2 + longitud xs

--Map - Aplica una función a cada elemento de los pares
myMap :: (a -> c) -> (b -> d) -> ListaPar a b -> ListaPar c d 
myMap _ _ [] = []
myMap f g ((a,b):xs) = (f a, g b) : myMap f g xs


--Sumar pares - Suma todos los pares de una lista
sumaPares :: (Num a, Num b) => ListaPar a b -> (a,b)
sumaPares [] = (0,0)
sumaPares ((a,b):xs) = (a + sumaa, b + sumab)
    where (sumaa, sumab) = sumaPares xs

--Filter pares - Filtra pares que cumplan cierta condición
myFilter :: ((a,b) -> Bool) -> ListaPar a b -> ListaPar a b
myFilter _ [] = []
myFilter d ((a,b):xs)
    | d (a,b)  = (a,b) : myFilter d xs
    | otherwise = myFilter d xs

-- Calcula la longitud de cualquier lista
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Calcula la diferencia de listas (elementos de la primera que no están en la segunda)
diferencia :: Eq a => [a] -> [a] -> [a]
diferencia [] _ = []
diferencia (x:xs) ys
    | contenido ys x = diferencia xs ys
    | otherwise        = x : diferencia xs ys

-- Verifica si un elemento está contenido en una lista
contenido :: Eq a => [a] -> a -> Bool
contenido [] _ = False
contenido (y:ys) x
    | x == y    = True
    | otherwise = contenido ys x

-- Invierte una lista
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]