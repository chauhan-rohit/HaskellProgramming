module Folds where
import Data.Char
import Data.Time
f1 = foldr (++) "" ["woot","WOOT","woot"]
f2 = foldr max [] (words "fear is the little death")
f3 = foldr (&&) True [False,True]
f4 = foldl (flip ((++) . show)) "" [1..5]
f5 = foldr const (ord 'a') [1..5]
f6 = foldr const '0' "tacos"

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
	, DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = [ x | DbDate x <- (filter isDbDate xs)]

isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate x) = True
isDbDate _  = False

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = [ x | DbNumber x <- (filter isDbNumber xs) ]

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber x) = True
isDbNumber _ = False

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr (\x y -> if x > y then x else y) b (filterDbDate xs)
    where
       b = (UTCTime (fromGregorian 0000 0 0) (secondsToDiffTime 00000))

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (\x y -> (+) x y) 0 (filterDbNumber xs)

avgDb :: [DatabaseItem] -> Double
avgDb xs = foldr (\x y -> (((fromIntegral x) :: Double) + y) / 2.0) 0.0 (filterDbNumber xs)

fibs = 1 : scanl (+) 1 fibs
fibs20 = take 20 fibs
--below gets while less than hundred but still is an infinite list
fibsLess100 = [x | x <- fibs,x < 100]
fibsLessHundred = takeWhile (<100) fibs

factorial =  scanl (*) 1 [2..]

fact5 =  take 5 factorial

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combinations :: String -> String -> [(Char,Char,Char)]
combinations st vw = [(x,y,x) | x <- st,y <- vw]

pcombinations :: String -> String -> [(Char,Char,Char)]
pcombinations st vw = [(x,y,x) | x <- st, y <- vw, x == 'p']

myOr :: [Bool] -> Bool
myOr = foldr (\x y -> if (y == False) then x else y) False

myOrPF :: [Bool] -> Bool
myOrPF = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> if b == False then f x else b) False xs

myElem :: (Eq a) => a -> [a] -> Bool
myElem x xs = myAny (==x) xs

myElemF :: (Eq a) => a -> [a] -> Bool
myElemF x xs = foldr (\a b -> if (b == False) then (a == x) else b) False xs

myReverse :: [a] -> [a]
myReverse = foldl (\xs x -> x:xs) []

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x xs -> (f x) : xs) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = foldr (\x xs -> if ((f x) == True) then (x:xs) else xs) [] xs

mySquish :: [[a]] -> [a]
mySquish = foldr (\xs b -> xs ++ b) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = foldr (\x b -> f x ++ b) [] xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap (\xs -> xs)

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x b -> if (f x b) == GT then x else b) (head xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (y:ys) = foldr (\x b -> if (f x b) == LT then x else b) y ys

