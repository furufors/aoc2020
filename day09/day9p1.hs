#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . preamble 25 . map read . lines

preamble :: Int -> [Int] -> Int
preamble i is = preamble' (take i is) (drop i is)
    where
        preamble' _  [    ] = error "No solution."
        preamble' as (b:bs) = if b `elem` (candidates as) then preamble' (tail as ++ [b]) bs else b
        candidates as = [a+b | a <- as, b <- as, a /= b]