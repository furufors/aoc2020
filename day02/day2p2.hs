#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Char

main :: IO ()
main = interact $ show . length . filter valid . map parse . lines

parse :: String -> (Int,Int,Char,String)
parse s = let low = takeWhile isDigit s
              hig = takeWhile isDigit . drop 1 . dropWhile isDigit $ s
              char = head $ drop 1 . dropWhile isDigit . drop 1 . dropWhile isDigit $ s
              pass = drop 4 . dropWhile isDigit . drop 1 . dropWhile isDigit $ s
          in (read low, read hig, char, pass)

valid :: (Int,Int,Char,String) -> Bool
valid (l,h,c,s) = (s!!(l-1) == c) /= (s!!(h-1) == c)