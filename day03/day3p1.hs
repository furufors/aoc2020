#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . ride 0 0 . lines

ride :: Int -> Int -> [String] -> Int
ride x y ss = let x' = x `mod` (length $ head ss)
              in if y >= length ss
                 then 0
                 else (if ss!!y!!x' == '.' then 0 else 1) + ride (x+3) (y+1) ss