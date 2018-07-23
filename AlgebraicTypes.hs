{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
module AlgebraicTypes where
data Price = Price Integer deriving (Eq,Show)
data Size = Size Integer deriving (Eq,Show)
data Manfacturer = Mini | Mazda | Tata deriving (Eq,Show)
data Airline = PapuAir | Catapult | TakeYourChanceUnited deriving (Eq,Show)
data Vehicle = Car Manfacturer Price | Plane Airline Size deriving (Eq,Show)
data Example = MakeExample deriving Show
data Example2 = MakeExample2 Int deriving Show
data OperatingSystem = GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows deriving (Eq,Show)
data ProgLang = Haskell | Agda | Idris | PureScript deriving (Eq,Show)
data Programmer = Programmer { os :: OperatingSystem,lang :: ProgLang} deriving (Eq,Show)
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq,Ord,Show)
class TooMany a where
    tooMany :: a -> Bool
instance TooMany Int where
   tooMany n = n > 42
instance TooMany (Int,String) where
   tooMany (x,y) = x > 42
instance TooMany (Int,Int) where
   tooMany (x,y) = (x+y) > 42
instance (Num a, TooMany a) => TooMany (a,a) where
   tooMany (x,y) = tooMany (x,y)
newtype Goats = Goats Int deriving (Eq,Show,TooMany)
myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 1000)
allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux,OpenBSDPlusNevermindJustBSDStill,Mac,Windows]
allLanguages :: [ProgLang]
allLanguages = [Haskell,Agda,Idris,PureScript]
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars xs = map isCar xs

getManu :: Vehicle -> Manfacturer
getManu (Car x _) = x

allProgrammers :: [Programmer]
allProgrammers = [getProgrammer x y | x <- allOperatingSystems,y <- allLanguages]

getProgrammer :: OperatingSystem -> ProgLang -> Programmer
getProgrammer x y= Programmer { os = x,lang = y}

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf)
	     1 
         (Node Leaf 4 Leaf)

mapExpected = 
    Node (Node Leaf 4 Leaf)
         2
         (Node Leaf 5 Leaf)
mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "test okay"
    else error "test failed"
preorder :: BinaryTree a -> [a]
preorder bt = 
    go bt [] 
        where
            go  Leaf xs  = xs
            go (Node left x right) xs = x : (go left (go right xs))
inorder :: BinaryTree a -> [a]
inorder bt =
    go bt []
        where
            go Leaf xs = xs
            go (Node left x right) xs = go left (x : (go right xs))
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b Leaf = b
foldTree f b (Node left a right) = f a (foldTree f (foldTree f b left) right)
testTree :: BinaryTree Integer
testTree = 
    Node (Node Leaf 1 Leaf)
	     2
		 (Node Leaf 3 Leaf)