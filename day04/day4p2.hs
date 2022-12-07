#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import Data.List.Split
import Data.Char

main :: IO ()
main = interact $ show . length . filter strict . filter valid . splitOn "\n\n"

valid :: String -> Bool
valid passport = all (\s -> isInfixOf s passport) ["ecl:", "pid:", "eyr:", "hcl:", "byr:", "iyr", "hgt:"]

strict :: String -> Bool
strict passport = all (\f -> f (passport ++ " ")) [ecl, pid, eyr, hcl, byr, iyr, hgt]

{-
    byr (Birth Year) - four digits; at least 1920 and at most 2002.
    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    hgt (Height) - a number followed by either cm or in:
        If cm, the number must be at least 150 and at most 193.
        If in, the number must be at least 59 and at most 76.
    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    pid (Passport ID) - a nine-digit number, including leading zeroes.
 -}

ecl, pid, eyr, hcl, byr, iyr, hgt :: String -> Bool
byr [] = False
byr ('b':'y':'r':':':d1:d2:d3:d4:e:ss) | all isDigit [d1,d2,d3,d4] = let year = read [d1,d2,d3,d4] in year >= 1920 && year <= 2002 && validEnd e
byr (s:ss) = byr ss
iyr [] = False
iyr ('i':'y':'r':':':d1:d2:d3:d4:e:ss) | all isDigit [d1,d2,d3,d4] = let year = read [d1,d2,d3,d4] in year >= 2010 && year <= 2020 && validEnd e
iyr (s:ss) = iyr ss
hgt [] = False
hgt ('h':'g':'t':':':d1:d2:'i':'n':e:ss) | all isDigit [d1,d2] = let height = read [d1,d2] in height >= 59 && height <= 76 && validEnd e
hgt ('h':'g':'t':':':d1:d2:d3:'c':'m':e:ss) | all isDigit [d1,d2,d3] = let height = read [d1,d2,d3] in height >= 150 && height <= 193 && validEnd e
hgt (s:ss) = hgt ss
eyr [] = False
eyr ('e':'y':'r':':':d1:d2:d3:d4:e:ss) | all isDigit [d1,d2,d3,d4] = let year = read [d1,d2,d3,d4] in year >= 2020 && year <= 2030 && validEnd e
eyr (s:ss) = eyr ss
hcl [] = False
hcl ('h':'c':'l':':':'#':d1:d2:d3:d4:d5:d6:e:ss) = all (`elem` (['0'..'9']++['a'..'f'])) [d1,d2,d3,d4,d5,d6] && validEnd e
hcl (s:ss) = hcl ss
ecl [] = False
ecl ('e':'c':'l':':':d1:d2:d3:e:ss) = [d1,d2,d3] `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] && validEnd e
ecl (s:ss) = ecl ss
pid [] = False
pid ('p':'i':'d':':':d1:d2:d3:d4:d5:d6:d7:d8:d9:e:ss) = all isDigit [d1,d2,d3,d4,d5,d6,d7,d8,d9] && validEnd e
pid (s:ss) = pid ss

validEnd :: Char -> Bool
validEnd c = c `elem` ['\r','\n','\t',' ']