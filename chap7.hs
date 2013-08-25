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

-- 2
-- Define the higher order functions all, any, takeWhile, dropWhile

-- 2.1 all
-- all p = and . map p

-- 2.2 any
-- any p =  or . map p

-- 2.3 takeWhile
-- takeWhile _ []      = []
-- takeWhile p (x:xs)  | p x = x: takeWhile p xs
--                     | otherwise = []

-- 2.4 dropWhile
-- dropWhile _ [] 	  = []
-- dropWhile p (x:xs) | not p x = x: dropWhile p xs
--                    | otherwise = []

-- 3
-- Redefine the functions map f and filter p using foldr.
-- f []     = v
-- f (x:xs) = x # f xs

-- interpreting sum this way gives
-- sum []     = 0
-- sum (x:xs) = x + sum xs

-- using the foldr recursion pattern gives
-- sum = foldr (+) 0

-- map f would look like
-- map = foldr (f) 0
-- foldr (\x xs -> f x:xs) []

-- filter p would look like
-- filer p = foldr (\x xs -> if p x then [x] else []) [] xs
-- i.e.  foldr (\a b -> if p a then [a] else []) [] [1,1,1,3]
-- []

-- 4
-- Using foldl, define a function that converts a decimal into int
-- i.e. >dec2int [2,3,4,5]
--      2345
dec2int :: [Int] -> Int
dec2int = foldl (\a b -> a*10 + b) 0

-- 5
-- Explain why the following definition is invalid:
-- sumsqreven = compose [sum, map (^2), filter even]
-- sum is inside the list comprehension, should foldr sum (or map)
-- over the resulting list comprehension.

-- 6
-- Define curry, that converts a function on pairs into a curried
-- function and uncurry.
--curry1 :: ((a,b) -> c) -> (a -> b -> c)
-- curry1 f = \x y -> f (x, y)

-- uncurry1 :: (a -> b -> c) -> ((a,b) -> c)
-- uncurry1 f = \(x,y) -> f x y

-- 7
--unfold :: a -> a -> a -> [a]
--unfold p h t x | p x = []
--               | otherwise = h x : unfold p h t (t x)

-- int2bin example
-- int2bin = unfold (==0) (`mod` 2) (`div` 2)

-- chop8 as redefined using unfold
-- chop8 = unfold (==[]) (take 8 bits) (drop 8 bits)
-- should be chop8 = unfold null (take 8) (drop 8)

-- map f redefined using unfold
-- map f = unfold null (f) (tail)
-- should be map f = unfold null (f . head) tail

-- iterate f redefined using unfold
-- iterate f = unfold (const False) id f -- not sure how const False fits in

-- 8
-- new encode will add a parity bit
addparity      :: [Bit] -> [Bit]
addparity bits = if sum bits `mod` 2 /= 0 then bits ++ [1]
                 else bits ++ [0] 

newencode :: String -> [Bit]
newencode = addparity . concat . map (make8 . int2bin . ord)

checkparity      :: [Bit] -> [Bit]
checkparity bits = if (sum (take (length bits - 1) bits) `mod` 2 == last bits) then take (length bits - 1) bits
	               else error "Oh baby"

newdecode :: [Bit] -> String
newdecode = map (chr . bin2int) . chop8 . checkparity

















