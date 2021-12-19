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

cols dat = parseFile' [] (length (head dat) - 1) dat
mostCommon = map (\col -> mostCommonBit 0 0 col)
leastCommon = map (\col -> leastCommonBit col)

parseFile rows =
    let
        mappings = mostCommon (cols rows)
        o2 = bin2dec $ reverse $ procO2 rows 0
        co2 = bin2dec $ reverse $ procCO2 rows 0
    in
        o2 * co2

procO2 [a] _ = a
procO2 acc idx =
    let
        mappings = mostCommon (cols acc)
        remaining = filter (\row -> row!!idx == mappings!!idx) acc
    in
    procO2 remaining (idx + 1)

procCO2 [a] _ = a
procCO2 acc idx =
    let
        mappings = leastCommon (cols acc)
        remaining = filter (\row -> row!!idx == mappings!!idx) acc
    in
    procCO2 remaining (idx + 1)

parseFile' acc (-1) dat = reverse acc
parseFile' acc idx dat = parseFile' (acc ++ [collectNth [] idx dat]) (idx - 1) dat

collectNth :: [Char] -> Int -> [[Char]] -> [Char]
collectNth acc n [] = acc
collectNth acc n (l:ls) =
    collectNth (acc ++ [(l!!n)]) n ls

mostCommonBit nZ nS [] = if nZ > nS then '0' else '1'
mostCommonBit nZ nS (c:cs) =
    case c of '1' -> mostCommonBit nZ (nS + 1) cs
              otherwise -> mostCommonBit (nZ + 1) nS cs

leastCommonBit cs =
    case mostCommonBit 0 0 cs of '0' -> '1'
                                 otherwise -> '0'

bin2dec [] = 0
bin2dec (x:xs) = (digitToInt x) + 2 * (bin2dec xs)
