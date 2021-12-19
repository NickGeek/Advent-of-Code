{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Maybe
import Text.Read hiding (step)
import Data.List
import Data.Ord
import Data.Char (digitToInt)

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let parsed = parseFile (lines raw)
    putStrLn $ show parsed

parseFile dat =
    let
        cols = parseFile' [] (length (head dat) - 1) dat
        gamma = bin2dec $ reverse $ map (\col -> mostCommonBit 0 0 col) cols
        epsilon = bin2dec $ reverse $ map (\col -> leastCommonBit col) cols
    in
        gamma * epsilon


parseFile' acc (-1) dat = reverse acc
parseFile' acc idx dat = parseFile' (acc ++ [collectNth [] idx dat]) (idx - 1) dat

collectNth :: [Char] -> Int -> [[Char]] -> [Char]
collectNth acc n [] = acc
collectNth acc n (l:ls) =
    collectNth (acc ++ [(l!!n)]) n ls

mostCommonBit nZ nS [] = if nS > nZ then '1' else '0'
mostCommonBit nZ nS (c:cs) =
    case c of '1' -> mostCommonBit nZ (nS + 1) cs
              otherwise -> mostCommonBit (nZ + 1) nS cs

leastCommonBit cs =
    case mostCommonBit 0 0 cs of '0' -> '1'
                                 otherwise -> '0'

bin2dec [] = 0
bin2dec (x:xs) = (digitToInt x) + 2 * (bin2dec xs)

-- mode = snd . maximumBy (comparing fst)
