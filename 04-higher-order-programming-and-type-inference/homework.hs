-- Exercise 1

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even


fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate f
  where f x
         | even x    = x `div` 2
         | otherwise = 3 * x + 1       

-- The first function was a direct translation. The second, however,
-- was doing two things at once: building the hailstone sequence
-- (recusrive calls) and summing up the number if it was even (first
-- guard of the second equation).


-- Exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)


foldTree :: [a] -> Tree a
foldTree = foldr f Leaf
   where f :: a -> Tree a -> Tree a
         f x Leaf = Node 0 Leaf x Leaf
         f x (Node h l nx r)
           | height l == height r = let newL = (f x l) in Node (max (height l) (height newL) + 1) newL nx r
           | height l > height r = Node h l nx (f x r)
           | height l < height r = Node h (f x l) nx r


height :: Tree a -> Integer
height Leaf = -1                -- because of (Node 0 Leaf x Leaf)
height (Node h _ _ _) = h


-- Exercise 3

xor :: [Bool] -> Bool
xor = foldr (/=) False


map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []


-- TODO: won't work on infinite lists because of reverse.

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)


-- Exercise 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map doubleAndIncrement . filter withoutForm . upTo
  where upTo n = [1..n]
        withoutForm x = null $ filter (hasForm x) $ cartProd [1..x] [1..x]
          where hasForm x (i,j) = i + j + 2 * i * j == x
        doubleAndIncrement x = 2 * x + 1


cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
