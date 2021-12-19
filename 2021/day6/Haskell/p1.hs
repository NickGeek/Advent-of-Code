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
    raw <- readFile "inputT.txt"
    let initialState = map (\num -> read (T.unpack num) :: Int) (T.splitOn "," (T.pack raw))
    let part1 = run initialState 18
    putStrLn $ show $ part1
    putStrLn $ show $ length part1

run fish 0 = fish
run fish n = run (step [] fish) (n - 1)

step res [] = res
step res (0:fs) = step (res ++ [6]) (fs ++ [9])
step res (f:fs) = step (res ++ [f - 1]) fs

test = [3,4,3,1,2]
