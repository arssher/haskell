import Data.Char

-- quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let smallerSorted = quicksort [a | a <- xs, a <= x];
		biggerSorted = quicksort [a | a <- xs, a > x]
	in
		biggerSorted ++ [x] ++ smallerSorted

-- factorial
factorial :: (Eq a, Num a) => a -> a
factorial 1 = 1
factorial n = n * factorial (n - 1)

--factorial'
factorial' :: (Eq a, Enum a, Num a) => a -> a
factorial' n = product[1..n]

--sum of odd squares, < 10000
pumpum = sum( takeWhile ( < 10000) (filter odd (map (^ 2) [1..])))

-- Collatz's series
collatzSeries :: Integer -> [Integer]
collatzSeries 1 = [1]
collatzSeries n
	| even n = n : collatzSeries (n `div` 2)
	| odd n = n : collatzSeries (3*n + 1)

-- How many of them longer than 15, if start numbers are 1..100?
howManyOfThem :: Int
howManyOfThem = length (filter isLongEnough (map collatzSeries [1..100]))
	where isLongEnough xs = (length xs) > 15
--or with lamba-expression:
howManyOfThem' :: Int
howManyOfThem' = length (filter (\xs -> length xs > 15) (map collatzSeries[1..100]))


--sum folding
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

--map through folding
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

--Caesar's encoding
encode :: Int -> String -> String
encode offset message = map (\c -> chr $ ord c + offset) message

--decoding
decode :: Int -> String -> String
decode offset code = encode (negate offset) code


--shapes
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
area :: Shape -> Float
area (Circle _ _ r) = pi * r^2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

data PointType = PointFunc Float Float deriving (Show)
data Shape' = Circle' PointType Float | Rectangle' PointType PointType deriving (Show)


--tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)
leaf :: a -> Tree a
leaf x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = leaf x
treeInsert x (Node a left right)
	| x == a = Node x left right
	| x < a = Node x (treeInsert x left) right
	| x > a = Node x left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
	| x == a = True
	| x < a = treeElem x left
	| x > a = treeElem x right

--types magic
--Ребятки-типы, принимающие один параметр, могут получить право называться функтором, если определят fmap:
class Functor' f where
	fmap' :: (a -> b) -> f a -> f b
--например, maybe желает стать таким:
instance Functor' Maybe where
	fmap' f (Just x) = Just (f x)
	fmap' f Nothing = Nothing
--списки тоже хотят
instance Functor' [] where
	fmap' f l = map f l

--Продолжаем разговор
--Ребятки, принимающие один параметр * и один параметр * -> *, могут получить дан То-фу
class Tofu t where
	tofu :: j a -> t a j
--Таких желающих немного, поэтому придумаем какой-нибудь Франк
data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
	tofu x = Frank x
