#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Data.List
import Data.Ord
import qualified Data.Map as M
data MaskVal = X | One | Zero deriving (Show, Eq)
type Mask = [MaskVal]
type BitVal = [Bool]
type Addr = (Int, Int)
data Instruction = M Mask | A Addr deriving (Show)
type Memory = M.Map Int Int

main :: IO ()
main = interact $ show . sum . M.elems . snd . foldl operation ([], M.empty) . map parseInstruction . lines

operation :: (Mask, Memory) -> Instruction -> (Mask, Memory)
operation (____,mem) (M mask) = (mask, mem)
operation (mask,mem) (A (addr,val)) = (mask, M.insert addr (maskInt mask val) mem)

toWait :: (Int, [Int]) -> [(Int, Int)]
toWait (t,bs) = map (\b -> (b, b - (t `mod` b))) bs

intToBitVal :: Int -> BitVal
intToBitVal i = reverse . take 36 $ reverse (intToBin i) ++ repeat False

intToBin :: Int -> BitVal
intToBin 0 = []
intToBin n = (intToBin (n`div`2)) ++ [n `mod` 2 == 1]

bitValToInt :: BitVal -> Int
bitValToInt bs =
    let constants = map (\x -> 2^x) $ reverse [0..35]
        f True  i = i
        f False i = 0
    in sum $ zipWith f bs constants

maskInt :: Mask -> Int -> Int
maskInt m i = bitValToInt $ maskBitVal m (intToBitVal i)

maskBitVal :: Mask -> BitVal -> BitVal
maskBitVal [] [] = []
maskBitVal _  [] = error "Unmatched bitval and mask"
maskBitVal [] _  = error "Unmatched bitval and mask"
maskBitVal (   X:xs) (b:bs) =     b:(maskBitVal xs bs)
maskBitVal ( One:xs) (b:bs) =  True:(maskBitVal xs bs)
maskBitVal (Zero:xs) (b:bs) = False:(maskBitVal xs bs)

parseInstruction :: String -> Instruction
parseInstruction input = case parse (try parseMask <|> try parseAddr) "parsein" input of
    Left err -> error $ show err
    Right a -> a

parseMask :: Parsec String () Instruction
parseMask = do
    _ <- string "mask = "
    mask <- many1 parseMaskVal
    return $ M mask

parseMaskVal :: Parsec String () MaskVal
parseMaskVal = try parseX <|> try parseOne <|> try parseZero

parseX    = string "X" >> return X
parseOne  = string "1" >> return One
parseZero = string "0" >> return Zero

parseAddr :: Parsec String () Instruction
parseAddr = do
    _ <- string "mem["
    addr <- read <$> many1 digit
    _ <- string "] = "
    value <- read <$> many1 digit
    return $ A (addr, value)