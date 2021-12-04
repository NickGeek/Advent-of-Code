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

play [] bs = error $ show $ bs
play (n:ns) bs =
    let
        boards = call n bs
        winners = check n boards 
    in
        case winners of
            Just winners ->
                if (length boards) == 1 then
                    calcScore n (head winners)
                else
                    play ns (boards \\ winners)
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
        hasWinner = (length leading) >= 1
    in
        if hasWinner then
            Just leading
        else
            Nothing
check' (row:rs) = (length (filter (\(cn, q) -> q) row)) == (length row) || check' rs
check' [] = False

calcScore n b = n * (sumUnmarked 0 b)

sumUnmarked :: Int -> [[(Int, Bool)]] -> Int
sumUnmarked acc [] = acc
sumUnmarked acc (row:rs) =
    let
        unmarked = filter (\(_, q) -> not q) row
        unmarkedNums = map (\(n, _) -> n) unmarked
    in
    sumUnmarked (acc + sum unmarkedNums) rs

tBoards = [[[(22,False),(13,False),(17,False),(11,False),(0,False)],[(8,False),(2,False),(23,False),(4,False),(24,False)],[(21,False),(9,False),(14,False),(16,False),(7,False)],[(6,False),(10,False),(3,False),(18,False),(5,False)],[(1,False),(12,False),(20,False),(15,False),(19,False)]],[[(3,False),(15,False),(0,False),(2,False),(22,False)],[(9,False),(18,False),(13,False),(17,False),(5,False)],[(19,False),(8,False),(7,False),(25,False),(23,False)],[(20,False),(11,False),(10,False),(24,False),(4,False)],[(14,False),(21,False),(16,False),(12,False),(6,False)]],[[(14,False),(21,False),(17,False),(24,False),(4,False)],[(10,False),(16,False),(15,False),(9,False),(19,False)],[(18,False),(8,False),(23,False),(26,False),(20,False)],[(22,False),(11,False),(13,False),(6,False),(5,False)],[(2,False),(0,False),(12,False),(3,False),(7,False)]]]
