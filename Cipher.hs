module Cipher where

import Data.Char

caesar :: [Char] -> [Char]
caesar [] = []
caesar (x:xs) = (chr.getCipheredInt) x : caesar xs

getCipheredInt :: Char -> Int
getCipheredInt c = if x > 96 then x else (x+96)
    where x = (((ord c) + 3) `mod` 122) :: Int

unCaesar :: [Char] -> [Char]
unCaesar [] = []
unCaesar (x:xs) = (chr.getunCipheredInt) x : unCaesar xs

getunCipheredInt :: Char -> Int
getunCipheredInt c = if x < 97 then (122 - (96 - x)) else x
    where x = (ord c) - 3