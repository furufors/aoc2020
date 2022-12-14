#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . contiguous . map read . lines

contiguous :: [Int] -> Int
contiguous xs = let magic = preamble 25 xs
                in contiguous' magic xs

contiguous' :: Int -> [Int] -> Int
contiguous' i [] = error "No solution"
contiguous' i is = case takeUpToMagic i is of
    Just n -> n
    Nothing -> contiguous' i (tail is)

takeUpToMagic :: Int -> [Int] -> Maybe Int
takeUpToMagic i is = let ints = dropWhile ((<i) . sum) $ map (\n -> take n is) [1..(length is)]
                     in if length ints >= 1 && (sum $ head ints) == i
                        then Just (minimum (head ints) + maximum (head ints))
                        else Nothing

preamble :: Int -> [Int] -> Int
preamble i is = preamble' (take i is) (drop i is)
    where
        preamble' _  [    ] = error "No solution."
        preamble' as (b:bs) = if b `elem` (candidates as) then preamble' (tail as ++ [b]) bs else b
        candidates as = [a+b | a <- as, b <- as, a /= b]