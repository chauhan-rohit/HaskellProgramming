main :: IO()
main = do
    x <- getLine
    let n = read x :: Int
    let nfib = getFibonacciNumber n
    putStrLn (show nfib)

getFibonacciNumber :: Int -> Integer
getFibonacciNumber n = fibs !! n
    where fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)