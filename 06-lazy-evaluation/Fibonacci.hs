import Data.List


-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


fibs1 :: [Integer]
fibs1 = map fib [0..]


-- Exercise 2

fibs2 = concatMap (\(a,b) -> [a,b]) $ iterate (\(a,b) -> (a+b, a+2*b)) (0,1)


-- Exercise 3

data Stream a = Cons a (Stream a)


streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs


instance Show a => Show (Stream a) where
  show stream = "[" ++ (content stream) ++ ",..]"
    where content = intercalate "," . map show . take 20 . streamToList


-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x


streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

  
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed $ streamFromSeed f (f seed)


-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed succ 0


ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap succ ruler)


interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)


-- Not a clever implementation of ruler

ruler' :: Stream Integer
ruler' = streamMap (f 0) (streamFromSeed succ 1)
  where f power x | x == 0 = power
                  | x `mod` 2 == 0 = f (power + 1) (x `div` 2)
                  | otherwise = power


-- TODO: Exercise 6


-- TODO: Exercise 7
