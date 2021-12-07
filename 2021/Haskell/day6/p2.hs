{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Maybe
import Text.Read hiding (step)
import Data.List
import qualified Data.Map as Map
import Data.Ord
import Data.Char (digitToInt)

main :: IO ()
main = do
    raw <- readFile "inputT.txt"
    let input = map (\num -> read (T.unpack num) :: Int) (T.splitOn "," (T.pack raw))
    let initialState = fillInitial Map.empty input
    -- let part1 = run initialState 256 -- 26984457539
    -- putStrLn $ show $ part1
    -- putStrLn $ show $ length part1
    putStrLn $ show $ initialState

type Counts = Map.Map Int Integer

fillInitial :: Counts -> [Int] -> Counts
fillInitial c [] = c
fillInitial s (v:vs) =
    let add1 new cur = cur + 1
    in fillInitial (Map.insertWith add1 v 1 s) vs

run counts 0 = counts
run counts d = 

-- run fish 0 = fish
-- run fish n = run (step [] fish) (n - 1)

-- step res [] = res
-- step res (0:fs) = step (res ++ [6]) (fs ++ [9])
-- step res (f:fs) = step (res ++ [f - 1]) fs

test = [3,4,3,1,2]
