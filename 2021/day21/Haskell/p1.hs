{-# LANGUAGE OverloadedStrings #-}

import Text.Read
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Data.Char
import Control.Monad

import Debug.Trace

main :: IO ()
main = do
    raw <- readFile "inputT.txt"
    let [player1, player2] = map (digitToInt . last) (lines raw)
    let (p1s, p2s, diceCount) = part1 player1 player2
    let part1Answer = (min p1s p2s) * diceCount
    putStrLn $ show $ part1 player1 player2
    putStrLn $ show $ part1Answer
    putStrLn $ show $ part2 player1 player2

part1 :: Int -> Int -> (Int, Int, Int)
part1 p1 p2 = part1' p1 p2 0 0 dd 0
part1' p1pos p2pos p1score p2score dd c =
    let
        p1NewPos = track!!(p1pos + (sum (take 3 dd)) - 1)
        p2NewPos = track!!(p2pos + (sum (take 3 (drop 3 dd))) - 1)
        p1NewScore = p1score + p1NewPos
        p2NewScore = p2score + p2NewPos
    in
        if p1NewScore >= 1000 then
            (p1NewScore, p2score, (c + 3))
        else if p2NewScore >= 1000 then
            (p1score, p2NewScore, (c + 3))
        else
            part1' p1NewPos p2NewPos p1NewScore p2NewScore (drop 6 dd) (c + 6)


dd = iterate (\v -> if v < 100 then v + 1 else 1) 1
track = iterate (\v -> if v < 10 then v + 1 else 1) 1

-- Part 2
-- part2 :: Integer -> Integer -> (Integer, Integer, Integer)
-- part2 p1 p2 = part2' p1 p2 0 0 (0, 0)
-- part2' p1pos p2pos p1score p2score (p1w, p2w) =
--     let
--         p1NewPos = track!!(p1pos + (1) - 1)
--         p2NewPos = track!!(p2pos + (sum (take 3 (drop 3 dd))) - 1)
--         p1NewScore = p1score + p1NewPos
--         p2NewScore = p2score + p2NewPos
--     in
--         if p1NewScore >= 21 then
--             (p1NewScore, p2score, (c + 3))
--         else if p2NewScore >= 21 then
--             (p1score, p2NewScore, (c + 3))
--         else
--             part2' p1NewPos p2NewPos p1NewScore p2NewScore (drop 6 dd) (c + 6)

data Winner = Player1 | Player2 deriving (Show, Eq)

part2 :: Int -> Int -> (Int, Int)
part2 p1pos p2pos =
    let 
        universes = foldMap (\p1r -> map (\p2r -> part2Cached p1pos p2pos 0 0 p1r p2r Map.empty)) qdie qdie
        (p1WinCount, p2WinCount) = foldr (\(sp1, sp2) (p1w, p2w) -> (sp1 + p1w, sp2 + p2w)) (0, 0) universes
    in
    (p1WinCount, p2WinCount)

type Cache = Map.Map (Int, Int, Int, Int) (Int, Int)

-- part2Cached :: Int -> Int -> Int -> Int -> Int -> Int -> Cache -> (Int, Int, Cache)
-- part2Cached p1pos p2pos p1score p2score p1roll p2roll cache =
--     case Map.lookup (p1pos, p2pos, p1score, p2score) cache of
--         Just res -> res
--         otherwise -> (Map.insert (p1pos, p2pos, p1score, p2score) (part2' p1pos p2pos p1score p2score p1roll p2roll cache) cache, Cache)


-- part2' :: Int -> Int -> Int -> Int -> Int -> Int -> Cache -> (Int, Int, Cache)
-- part2' p1pos p2pos p1score p2score p1roll p2roll cache =
--     let
--         p1NewPos = track!!(p1pos + p1roll - 1)
--         p2NewPos = track!!(p2pos + p2roll - 1)
--         p1NewScore = p1score + p1NewPos
--         p2NewScore = p2score + p2NewPos
--     in
--     if p1NewScore >= 21 then
--         (1, 0)
--     else if p2NewScore >= 21 then
--         (0, 1)
--     else
--         let
--             universes = foldMap (\p1r ->
--                                     map (\p2r -> part2' p1NewPos p2NewPos p1NewScore p2NewScore p1r p2r)
--                                 ) qdie qdie
--             (p1WinCount, p2WinCount) = foldr (\(sp1, sp2) (p1w, p2w) -> (sp1 + p1w, sp2 + p2w)) (0, 0) universes
--             winner = if p1WinCount > p2WinCount then Player1 else Player2
--         in
--         (p1WinCount, p2WinCount)

qdie = map sum qdiePerms

qdiePerms = replicateM 3 [1..3]
-- qdiePerms = nub $ (permutations [1..3])
--              ++ (permutations [1,1,1])
--              ++ (permutations [1,1,2])
--              ++ (permutations [1,2,2])
--              ++ (permutations [1,1,3])
--              ++ (permutations [1,3,3])
--              ++ (permutations [2,2,2])
--              ++ (permutations [2,2,1])
--              ++ (permutations [2,1,1])
--              ++ (permutations [2,2,3])
--              ++ (permutations [2,3,3])
--              ++ (permutations [3,3,3])
--              ++ (permutations [3,3,1])
--              ++ (permutations [3,1,1])
--              ++ (permutations [3,3,2])
--              ++ (permutations [3,2,2])
