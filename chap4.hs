-- Determine if a char is a number
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

-- Decide if a number is even
isEven :: Integral a => a -> Bool
isEven a = a `mod` 2 == 0

-- new const; understanding const...
newConst :: a -> (b -> a)
newConst x = \_ -> x

-- returns the first odd integers
odds :: Int -> [Int]
odds n = map f [0..n-1]
	where f x = x * 2 + 1

-- 4.8 exercises
-- 1
-- halve will split an even-lengthed list into two halves
-- halve with guards returns "Non-exhaustive patterns in function"
-- if I use [x].  Instead if I use xs it works.  Go figure.
halve :: [a] -> ([a], [a])
halve xs | length xs `mod` 2 == 0 = splitAt (length xs `div` 2) xs
		  | otherwise = ([],[])

-- halve using a conditional
-- returns with "Non-exhaustive patterns" likely because of the
-- brackets
-- halve [x] = if length [x] `mod` 2 == 0 
--	then splitAt (length [x] `div` 2) [x] else ([x], [x])

-- 2
-- safetail will function like the library function tail but will
-- handle the empty list by mapping it to itself

-- conditional version
safetailc :: [a] -> [a]
safetailc xs = if null xs then xs
			   else tail xs
-- guarded version
safetailg :: [a] -> [a]
safetailg xs | length xs /= 0 = tail xs
			 | otherwise = xs

-- pattern matching version
-- if the patterns are in reverse order, matching overlaps
safetailp :: [a] -> [a]
safetailp [] = []
safetailp xs = tail xs