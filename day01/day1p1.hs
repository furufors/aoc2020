#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . find . map read . lines

find :: [Int] -> Int
find [    ] = 0
find (a:as) = case [a * i | i <- as, a+i == 2020] of
                [   ] -> find as
                (s:_) -> s