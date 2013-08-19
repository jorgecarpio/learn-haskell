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

-- 2.  Show how length [1,2,3], drop 3 [1,2,3,4,5], and init [1,2,3] are
-- evaluated.
-- length :: [a] -> Int
-- length	[] = 0
-- length (_:xs) = 1 + length xs

-- length [1,2,3]
-- = {applying length}
--   1 + length [2,3]
-- = {applying length}
--   1 + (1 + length [3])
-- = {applying length}
--   1 + (1 + (1 + length []))
-- = {applying length}
--   1 + (1 + (1 + (0)))
-- = {applying (+)}
--   3

-- drop :: Int -> [a] -> [a]
-- drop 0 xs = xs
-- drop (n + 1) [] = []
-- drop (n + 1) (_:xs) = drop n xs

-- drop 3 [1,2,3,4,5]
-- = {applying drop}
-- drop 2 [2,3,4,5]
-- = {applying drop}
-- drop 1 [3,4,5]
-- = {applying drop}
-- drop 0 [4,5]
-- = {applying drop}
-- [4,5]

-- init :: [a] -> [a]
-- init [_] = []
-- init (x:xs) = x: init xs

-- init [1,2,3]
-- = {applying init}
-- [1: init [2,3]]
-- = {applying init}
-- [1: 2: init [3]]
-- = {applying init}
-- [1: 2: []]
-- [1,2]

-- 3.  Withou looking at the definitions from the standard prelude, define the 
-- folowing library functions using recursion:

-- Decide if all logical values in a list are True:
and3 :: [Bool] -> Bool
and3 [] = True
and3 (x:xs) = x && and3 xs

-- Concatenate a list of lists:
concat3 :: [[a]] -> [a]
concat3 [] = []
concat3 [[]] = []
-- concat3 [(x:xs)] = [x] ++ concat3 [xs]
-- concat3 [(x:xs)] = xs --> concat3 [[1,2]] --> [2]
-- concat3 [x:xs] = xs --> concat3 [[1,2]] --> [2]
-- concat3 (x:xs) = x --> concat3 [[1,2]] --> [1,2]
concat3 (x:xs) = x ++ concat3 (xs)

-- Produce a list with n identical elemnts:
replicate3 :: Int -> a -> [a]
replicate3 0 _ = []
replicate3 1 x = [x]
replicate3 (n+1) x = [x] ++ replicate3 n x

-- Select the nth element of a list (!!):
selectnth :: [a] -> Int -> a
selectnth (x:xs) 0 = x
selectnth (xs) (n+1) = selectnth (tail xs) n

-- Decide if a value is an element of a list (elem):
isamember :: Eq a => a -> [a] -> Bool
isamember _ [] = False
isamember x [y] = if x == y then True else False
-- This one don't work.
-- isamemeber x (y:ys) = if x == y then True else isamember x ys
isamember x (y:ys) | x == y = True
                   | otherwise = isamember x ys

-- 4. Define a recursive function merge that merges two sorted lists
-- to give a single sorted list.  
-- For example:
-- > merge [2,5,6] [1,3,4]
-- [1,2,3,4,5,6]
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x: merge xs (y:ys)
					| otherwise = y: merge (x:xs) ys

-- 5. Using merge, define a recursive function msort that implements
-- merge sort, in which the empty list and singleton lists are already
-- sorted, and any other list is sorted by merging together the
-- two lists that result from sorting the two halves of the list
-- separately.
-- Hint: first define a function halve that splits a list into two
-- halves whose length differs by at most one.

halve    :: [a] -> ([a],[a])
-- halve xs = [(take ((length xs `div` 2)) xs, drop ((length xs `div` 2)) xs)]
halve xs = splitAt (length xs `div` 2) xs

msort    :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs) where (ys, zs) = halve xs

-- 6
-- Using the 5 step process, define the library functions that
-- calculate the sum of a list of numbers, take a given number of 
-- elements from the start of a list, and select the last element
-- of a non-empty list.

-- 6.1
-- calculate the sum of a list of numbers
sumlist :: [a] -> Num a => a
sumlist [] = 0
sumlist (x:xs) = x + (sumlist xs)