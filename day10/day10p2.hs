#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List

main :: IO ()
main = interact $ show . calculate . sort . addSocket . map read . lines

calculate :: [Int] -> Int
calculate is = memoized_cal 0
    where
        memoized_cal = (map calculate' [0 ..]!!)
            where
                calculate' i = if i == maximum is then 1 else sum [ memoized_cal e | e <- [f | f <- is, (f - i) `elem` [1,2,3]]]

addSocket :: [Int] -> [Int]
addSocket is = 0:(maximum is + 3):is