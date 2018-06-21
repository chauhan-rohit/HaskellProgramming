module MyListFunctions where
import Data.Char
-- the below function removes articles (a,an,the) from the sentence and returns list of words
myFilter :: [Char] -> [[Char]]
myFilter s = filter (\x -> (elem x ["a","an","the"]) == False) (words s)

myzip1 :: [a] -> [b] -> [(a,b)]
myzip1 _ [] = []
myzip1 [] _ = []
myzip1 (x:xs) (y:ys) = (x,y) : myzip1 xs ys

getLengthOfSmallerList :: [a] -> [b] -> Int
getLengthOfSmallerList xs ys= if (length xs) > (length ys) then length ys else length xs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x:xs) (y:ys) = (f x y) : myZipWith f xs ys

myzip2 :: [a] -> [b] -> [(a,b)]
myzip2 xs ys = myZipWith (,) xs ys

removeLower :: [Char] -> [Char]
removeLower s = filter isUpper s

removeLower2 :: [Char] -> [Char]
removeLower2 s = [x | x <- s,isUpper x]

capitalizeFirst :: [Char] -> [Char]
capitalizeFirst s = (toUpper (head s)) : drop 1 s

capitalizeFirstP :: [Char] -> [Char]
capitalizeFirstP (x:xs) = (toUpper x) : xs

capitalizeFirstPR :: [Char] -> [Char]
capitalizeFirstPR [] = []
capitalizeFirstPR (x:xs) = (toUpper x) : (capitalizeFirstPR xs)

capitalizeAll :: [Char] -> [Char]
capitalizeAll s = map toUpper s

capitalizeAllRecursive :: [Char] -> [Char]
capitalizeAllRecursive s =
    go s (length s) []
    where
        go s l xs 
            | l == 0 = reverse xs
            | otherwise = go (tail s) (l-1) ((toUpper (head s)) : xs)
			
getcapitalizeFirst :: [Char] -> Char
getcapitalizeFirst s = toUpper (head s)

getcapitalizeFirst2 :: [Char] -> Char
getcapitalizeFirst2 s = (toUpper.head) s

getcapitalizeFirst3 :: [Char] -> Char
getcapitalizeFirst3 = (toUpper.head)

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if (x == True) then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if (f x) == True then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = if (a == x) then True else myElem a xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 a xs = any (==a) xs

myReverse :: [a] -> [a]
myReverse xs = go xs []
    where
        go [] rs = rs
        go (x:xs) rs = go xs (x:rs)
		
mySquish :: [[a]] -> [a]
mySquish [] = []
mySquish (x:xs) = x ++ mySquish xs
		
mySquishMap :: (a -> [b]) -> [a] -> [b]
mySquishMap _ [] = []
mySquishMap f (x:xs) = (f x) ++ mySquishMap f xs 

squishAgain :: [[a]] -> [a]
squishAgain xs = mySquishMap id xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = go f xs (head xs)
    where
        go _ [] z = z
        go f (x:xs) z = if (f x z) == GT then go f xs x else go f xs z		

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = go f xs (head xs)
    where
        go _ [] l = l
        go f (x:xs) l = if (f x l) == LT then go f xs x else go f xs l

myMaximum :: Ord a => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: Ord a => [a] -> a
myMinimum xs = myMinimumBy compare xs		
