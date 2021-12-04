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
    let (rawDrawn:rawBoards) = lines raw
    let drawn = map (\num -> read (T.unpack num) :: Int) (T.splitOn "," (T.pack rawDrawn))
    let parsed = parseFile [] rawBoards
    let res = play drawn parsed
    putStrLn $ show res

play [] _ = 0
play (n:ns) bs =
    let
        boards = call n bs
        winner = check n boards 
    in
    case winner of Just score -> score
                   otherwise -> play ns boards

parseFile acc (lSep:l1:l2:l3:l4:l5:ls) = parseFile (acc ++ [parseBoard l1 l2 l3 l4 l5]) ls
parseFile acc _ = acc

parseBoard l1 l2 l3 l4 l5 = [parseBoard' l1, parseBoard' l2, parseBoard' l3, parseBoard' l4, parseBoard' l5]
parseBoard' :: String -> [(Int, Bool)]
parseBoard' row = map (\num -> (read num :: Int, False)) (words row)

call n bs = map (\b -> map (\row -> map (\(cn, q) -> if cn == n then (cn, True) else (cn, q)) row) b) bs

check n bs =
    let
        leading = filter (\b -> check' b || check' (transpose b)) bs
        hasWinner = (length leading) == 1
    in
        if hasWinner then
            Just ((sumUnmarked 0 (head leading)) * n)
        else
            Nothing
check' (row:rs) = (length (filter (\(cn, q) -> q) row)) == (length row) || check' rs
check' [] = False

sumUnmarked :: Int -> [[(Int, Bool)]] -> Int
sumUnmarked acc [] = acc
sumUnmarked acc (row:rs) =
    let
        unmarked = filter (\(_, q) -> not q) row
        unmarkedNums = map (\(n, _) -> n) unmarked
    in
    sumUnmarked (acc + sum unmarkedNums) rs
