main :: IO()
main = do
    number <- getLine
    let n = (read number :: Integer)
    alldigits <- getLine
    let ls = createSequence (read number :: Integer) alldigits
    let (largest,second) = findLargestAndSecond ls
    putStrLn (show (largest * second))


createSequence :: Integer -> String -> [Integer]
createSequence n line =
    go n line []
    where
        go n l ls
            | n == 0 = ls
            | otherwise = go (n - 1) (drop 1 (dropWhile (/= ' ') l)) ((read (takeWhile (/= ' ') l) :: Integer) : ls)
			
findLargestAndSecond :: [Integer] -> (Integer,Integer)
findLargestAndSecond ls = 
   go ls (0,0)
       where
           go [] (f,s) = (f,s)
           go (x:xs) (f,s) = go (xs) (findLargerTwoOfThree x f s)


findLargerTwoOfThree :: Integer -> Integer -> Integer -> (Integer,Integer)
findLargerTwoOfThree e f s
    | e > f = (e,f)
    | e > s = (f,e)
    | otherwise = (f,s)
