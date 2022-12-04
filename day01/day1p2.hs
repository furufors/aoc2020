#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . find . map read . lines

find :: [Int] -> Int
find [    ] = 0
find (a:as) = case [a * i * j | i <- as, j <- as, i /= j, a+i+j == 2020] of
                [   ] -> find as
                (s:_) -> s