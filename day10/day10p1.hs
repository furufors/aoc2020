#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List

main :: IO ()
main = interact $ show . calculate . sort . map read . lines

calculate :: [Int] -> Int
calculate is = let is' = map (\(a,b) -> a - b) $ zip (is++[maximum is + 3]) (0:is)
                   ones = length $ filter (==1) is'
                   threes = length $ filter (==3) is'
               in ones * threes