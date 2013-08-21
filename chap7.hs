import Data.Char

-- Chapter 7

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- map is a higher order function
-- map :: (a -> b) -> [a] -> [b]
-- map f xs = [f x | x <- xs]

-- map can take itself and be applied to nested lists
-- map (map (+1)) [[1,2,3], [4,5]]
   -- {applying the outer map}
   -- [map (+1) [1,2,3], map (+1) [4,5]]
   -- {applying map}
   -- [[2,3,4], [5,6]]

-- map defined using recursion
-- map f [] = []
-- map f (x:xs) = f x : map f xs

-- filter returns a list of values that match a predicate
-- filter :: (a -> Bool) -> [a] -> [a]
-- filter p xs = [x | x <- xs, p x]

-- filter defined using recursion for reasoning purposes
-- filter p []                 = []
-- filter p (x:xs) | p x       = x: filter p xs
--                 | otherwise = filter p xs

-- returns the sum of the squares of even integers
sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

-- foldr fold right definition using recursion
-- foldr            :: (a -> b -> b) -> b -> [a] -> b
-- foldr f v []     = v
-- foldr f v (x:xs) = f x (foldr f v xs)

-- function composition
-- f composed wih g is the function that takes an arg x, applies g to it
-- then applies x to the result.
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- f . g = \x -> f (g x)

-- some functions rewritten using . (notice lack of arg)
-- odd n = not (even n)
-- odd = not . even
-- twice f x = f ( f x)
-- twice f = f . f
-- composition is associative, hence no parens needed
-- sumsqreven ns = sum (map (^2) (filter even ns))
-- sumsqreven = sum . map (^2) . filter even

-- Binary numbers are reversed here: 1101 is actualy 1011
type Bit = Int
bin2int :: [Bit] -> Int
-- bin2int bits = sum [w * b| (w,b) <- zip weights bits]
--			 where weights = iterate (*2) 1
bin2int = foldr (\x y -> x + 2 * y) 0

-- Implemented using recursion
int2bin    :: Int -> [Bit]
int2bin 0  = []
int2bin n = n `mod` 2: int2bin (n `div` 2)

-- Makes a binary number at least 8 bits
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- Encodes a sring of characters to binary after converting each Unicode
-- character to its corresponding number, then to binary
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

-- To decode a list of bits produced using encode, first we needed
-- a function to chop a list into 8-bit numbers
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- Now you can decode a list of bits as a string of chars.
decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

-- transmit is a function that simulates transmission of a string of 
-- characters as a list of bits via the identity function
transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- Chapter 7 Exercises
-- 1
-- Show how the list comprehension [f x | x <- xs, p x] can be
-- re-expressed using higher order functions map and filter.
-- map f (filter p xs)

