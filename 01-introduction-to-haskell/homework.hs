-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits n = revIntegerList (toDigitsRev n)


toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0 = n `mod` 10 : toDigitsRev (n `div` 10)
  | otherwise = []


revIntegerList :: [Integer] -> [Integer]
revIntegerList [] = []
revIntegerList (x:xs) = revIntegerList xs ++ [x]


-- toDigits 1234 == [1,2,3,4]
-- toDigitsRev 1234 == [4,3,2,1]
-- toDigits 0 == []
-- toDigits (-17) == []


-- Exercise 2

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x:y:xs) = [x, 2 * y] ++ doubleEveryOtherRev xs


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = revIntegerList (doubleEveryOtherRev (revIntegerList xs))


-- doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- doubleEveryOther [1,2,3] == [1,4,3]


-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumIntegerList (toDigits x) + sumDigits xs


sumIntegerList :: [Integer] -> Integer
sumIntegerList [] = 0
sumIntegerList (x:xs) = x + sumIntegerList xs


-- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22


-- Exercise 4

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0


-- validate 4012888888881881 = True
-- validate 4012888888881882 = False


-- Exercise 5

type Peg = String


type Move = (Peg, Peg)


hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0 = []
  | otherwise = hanoi (n - 1) a c b ++ [(a,b)] ++ hanoi (n - 1) c b a


-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]


-- Exercise 6

-- NOTE: hanoi4 is an adaptation of the Frame–Stewart algorithm for 4
-- pegs. The algorithm can be found on Wikipedia.
-- https://en.wikipedia.org/wiki/Tower_of_Hanoi#Frame.E2.80.93Stewart_algorithm

-- NOTE: hanoi4 moves discs from the first peg (a) to the last peg (d)
-- while hanoi moves discs from the first peg (a) to the second peg
-- (b).

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
  | n <= 0 = []
  | otherwise = hanoi4 (idealK n) a d c b
      ++ hanoi (n - idealK n) a d c
      ++ hanoi4 (idealK n) b a c d


idealK :: Integer -> Integer
idealK n = n - round (sqrt (2 * fromIntegral n + 1)) + 1


-- length (hanoi 15 "a" "b" "c") == 32767
-- length (hanoi4 15 "a" "b" "c" "d") == 129
