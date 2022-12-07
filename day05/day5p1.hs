#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . maximum . map calculate . lines

calculate :: String -> Int
calculate (a:b:c:d:e:f:g:h:i:j:_) = (x a * 64 + x b * 32 + x c * 16 + x d * 8 + x e * 4 + x f * 2 + x g * 1) * 8 + (y h * 4 + y i * 2 + y j * 1)

x :: Char -> Int
x 'F' = 0
x 'B' = 1

y :: Char -> Int
y 'L' = 0
y 'R' = 1