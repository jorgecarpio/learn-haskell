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
