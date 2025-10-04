module Practica02 where

import Data.List (union, intersect, (\\))

--BINARIOS
data Bit = O | I 
        deriving (Show, Eq)

type Binario = [Bit]

--BINARIOS

bitToInt :: Bit -> Int
bitToInt O = 0
bitToInt I = 1

intToBit :: Int -> Bit
intToBit 0 = O
intToBit 1 = I

toDecimal :: Binario -> Int
toDecimal [] = 0
toDecimal (x:xs) = (bitToInt x * 2^(length xs)) + toDecimal xs

toBin :: Int -> Binario
toBin 0 = []
toBin x = toBin (x `div` 2) ++ [intToBit (x `mod` 2)]

suma :: Binario -> Binario -> Binario
suma [] [] = []
suma [] y = y
suma x [] = x
suma x y = toBin ( toDecimal x + toDecimal y)

--LISTAS

palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reverse xs 

--Funcion principal que calcula la diferencia simetrica (Aquellos elementos que esten en la union pero no en la interseccion)
diferenciaSimetrica :: Eq a => [a] -> [a] -> [a]
diferenciaSimetrica xs ys = (xs `union` ys) \\ (xs `intersect` ys)

--Conjunto potencia
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia = undefined


--LISTAS DE LONGITUD PAR
--Esta va de regalo
type ListaPar a b = [(a,b)]

--Longitud
longitud :: ListaPar a b -> Int
longitud = undefined

--Map
myMap :: (a -> c) -> (b -> d) -> ListaPar a b -> ListaPar c d 
myMap = undefined

--Sumar pares
sumaPares :: ListaPar a b -> (a,b)
sumaPares = undefined

--Filter pares
myFilter :: ((a,b) -> Bool) -> ListaPar a b -> ListaPar a b
myFilter = undefined