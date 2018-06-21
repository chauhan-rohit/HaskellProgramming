import Data.Array
main :: IO()
main = do
    x <- getLine
    let num = read x :: Int
    let lastdigitFib = getFibLastDigit num
    putStrLn (show lastdigitFib)

getFibLastDigit :: Int -> Integer
getFibLastDigit num = go num
    where
        go 0 = 0
        go 1 = 1
        go n = (fibs ! (n - 1) + fibs ! (n - 2)) `mod` 10
        fibs = Data.Array.listArray (0,num) [(go x) `mod` 10 | x <- [0..num]] 