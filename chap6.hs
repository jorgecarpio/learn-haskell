-- Chapter 6
-- Must enable n + k patterns
{-# LANGUAGE NPlusKPatterns #-}

insert                        :: Ord a => a -> [a] -> [a]
insert x [] 		          = [x]
insert x (y : ys) | x <= y    = x : y : ys
				  | otherwise = y : insert x ys

-- This function implements insertion sort, in which an empty list is
-- already sorted, and any non-empty list is sorted by inserting its
-- head into the list that results from sorting its tail

isort        :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

-- 6.4 Multiple recursion
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (n + 2) = fibonacci n + fibonacci (n + 1)

-- Exercises
-- 1.  Define the exponentiation operator ^ for non-negative integers using the 
-- same pattern of recursion as the multiplication operator *, and show how 
-- 2^3 is evaluated using your definition.
-- (*) :: Int -> Int -> Int
-- m * 0 = 0
-- m * (n + 1) = m + (m * n)

expo           :: Int -> Int -> Int
expo m 0       = 1
expo 0 _       = 0
expo m (n + 1) = m * (expo m n)
