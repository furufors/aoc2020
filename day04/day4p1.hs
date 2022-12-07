#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import Data.List.Split
import Data.Char

main :: IO ()
main = interact $ show . length . filter valid . splitOn "\n\n"

valid :: String -> Bool
valid passport = all (\s -> isInfixOf s passport) ["ecl:", "pid:", "eyr:", "hcl:", "byr:", "iyr", "hgt:"]