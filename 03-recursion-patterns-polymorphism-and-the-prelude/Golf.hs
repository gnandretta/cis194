module Golf where

import Data.List


-- Exercise 1

skips :: [a] -> [[a]]
skips xs = map f [1..(length xs)]
  where f i = let p (x,n) = mod n i == 0
              in map fst (filter p (zip xs [1..]))


-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map fst (filter p (zip (drop 1 xs) (zip xs (drop 2 xs))))
  where p (x,(a,b)) = x > a && x > b


-- Exercise 3

histogram :: [Integer] -> String
histogram xs = concat (intersperse "\n" graph) ++ "\n==========\n0123456789\n"
  where graph = let p str = str /= "          "
                in filter p (transpose (map bar [0..9]))
        bar i = let f x = if x == i then '*' else ' '
                in sort (map f xs)
