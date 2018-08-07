module VigenereCipher where
import Data.Char
key :: [Char]
key = "ally"

keylength :: Int
keylength = length key

vigenereEncryption :: [Char] -> [Char]
vigenereEncryption xs =
    go xs 0
        where
            go [] _ = []
            go (l:ls) pos
                |l == ' ' = l : go ls pos
                |otherwise = chr (if z > 96 then z else (z+97)) : go ls (if pos == (keylength - 1) then 0 else (pos + 1))
                where z = getShiftedOrder l ((getShifts.getChrOrdInKey) pos)				

getChrOrdInKey :: Int -> Int
getChrOrdInKey p = ord (key !! p)

getShifts :: Int -> Int
getShifts c = c `mod` 97

getShiftedOrder :: Char -> Int -> Int
getShiftedOrder l o = if a > 122 then a `mod` 122 else a
    where a = ((ord l) + o)
	
getOriginalFromVigenere :: [Char] -> [Char]
getOriginalFromVigenere xs =
    go xs 0
        where
            go [] _ = []
            go (l:ls) pos
                | l == ' ' = l : go ls pos
                | otherwise = chr (if z < 97 then (122 - (97-z)) else z) : go ls (if pos == (keylength - 1) then 0 else (pos + 1))
                where z = ord l - ((getShifts.getChrOrdInKey) pos)
