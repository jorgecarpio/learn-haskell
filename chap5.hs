-- List Comprehensions
-- Example
-- > [x^2 | x <- [1..5]]
-- | means "such that"
-- <- means "is drawn from"
-- x <- [1..5] is a generator

-- Guards
-- Example
-- > [x | x <- [1..10], even x]
-- This produces a list of even numbers because the ", even x"
-- is a guard that "filters" the result.

import Data.Char

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- An integer greater than one is prime if its only positive factors
-- are one and itself.  Producing this
prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

-- This function will find pairs of tuples in a list that match a key
-- find :: Eq a => a -> [(a,b)] -> [b]
-- find k t = [v | (k', v) <- t, k==k']

-- This returns a list of pairs of adjacent elements from a source list.
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

-- Determines if a list is sorted using pairs
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

-- Uses zip to return all the positions at which a value occurs
-- positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = [i | (x', i) <- zip xs [0..n], x==x']
--	where n = length xs - 1

-- Since strings are lists, you can use list comprehensions.
-- This counts how many lowercase letters are in a string
lowers :: String -> Int
lowers xs = length[x| x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [x'|x' <- xs, x==x']

--Convert lowercase letters 'a-z' to integers 0-25 |]
let2int :: Char -> Int
let2int c = ord c - ord 'a'

--Reverse
int2let :: Int -> Char
int2let n = chr(ord 'a' + n)

--Shift applies a factor to lowercase letters wrapping at end of 
--alphabet.
shift :: Int -> Char -> Char
shift n c | isLower c = int2let((let2int c + n)`mod` 26)
		  | otherwise = c

--Encode uses shift with a string comprehension.
encode :: Int -> String -> String
encode n xs = [shift n x |x <- xs]

--Letter frequencies in English
table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0,
		 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

--Calculates the percentage of one integer with respect to another
percent :: Int -> Int -> Float
percent n m = (fromIntegral n /fromIntegral m) * 100

--Return the frequency table for any string
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
		  where n = lowers xs

--chi-square statistic
-- chisqr :: [Float] -> [Float] -> Float
-- chisqr os es = sum[((o - e)^2)/e | (o, e) <- zip os es]

--rotates a list n spaces, wrapping around to the start
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
		   where
		   		factor = head (positions(minimum chitab) chitab)
		   		chitab = [chisqr (rotate n table') table | n <- [0..25]]
		   		table' = freqs xs

-- Exercise 1
-- Using list comprehension, give an expression that 
-- calculates the sum of the first 100 squares.
sumsquares :: Int -> Int
sumsquares n = sum [ x^2 | x <-[1..n]]

-- Exercise 2
-- Show how the library function replicate can be defined as a list
-- comprehension.
replicateLC :: Int -> a -> [a]
replicateLC n a = [a|_ <-[0..n-1]]

-- Exercise 3 |]
-- A triple (x,y,z) of positive integers can be termed pythagorean if 
-- x^2 + y^2 = z^2.  Using list comprehension, define a function that 
-- returns the list of all pythagorean triples whose components
-- are at most at a given limit (i.e. > pyths 10 returns [(3,4,5),
--	(4,3,5), (6,8,10), (8,6,10)]).
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], 
			z <- [1..n], z^2 == x^2 + y^2]

-- Exercise 4
-- This function should return the list of all the perfect numbers up to
-- a given limit.  A number (positive integer) is perfect if it equals the 
-- sum of its factors.

-- factors 6 gives [1,2,3,6]
-- reverse (tail (reverse (factors 6)))
-- gives [1,2,3] -- the beginning of the list

-- sum(reverse (tail (reverse (factors 6)))) gives 6

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (reverse (tail (reverse (factors x))))]

-- Exercise 5
-- Show this single comprehension [(x,y) | x <- [1,2,3], y <- [4,5,6]]
-- can be re-expressed using two comprehensions with single generators.
singlecomp :: Int -> [(Int, Int)]
singlecomp n = [(x,y) | x <- [1..n], y <- [1..n]]

twocomps :: Int -> [(Int, Int)]
-- Here the first list comprehension [(a,b) | a <- [1..n]] will determine
-- the series [(1, b), (2, b), (3, b)] for n of 3
-- the second generator passes b to the first generator and in the above
-- example will pass in a 1 for [(1,1), (2,1), (3,1)] and so on.
-- concat is simply used to convert the resulting [[]] to a [].
twocomps n = concat[[ (a,b) | a <- [1..n]] | b <- [1..n]]

-- Exercise 6
-- Redefine the function positions using find.

-- This function will find pairs of tuples in a list that match a key
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k==k']
-- *Main> find 1 [(1, 'a'), (2, 'b')]
-- "a"

-- Uses zip to return all the positions at which a value occurs
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x==x']
	where n = length xs - 1
-- *Main> positions 9 [9, 8, 7, 6]
-- [0]

newpositions :: Char -> [Char] -> [Int]
newpositions x xs = find x (zip xs [0..n])
 	where n = length xs - 1

-- Exercise 7
-- The scalar product of two integer lists xs and ys of length n
-- is given by the sum of the products of corresponding integers.
-- Show how a list comprehension can be used to define the following
-- function, using chisqr as an example
-- > scalarproduct [1,2,3] [4,5,6]
-- > 32
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum[((o - e)^2)/e | (o, e) <- zip os es]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct is js = sum [(i * j)| (i, j) <- zip is js]

