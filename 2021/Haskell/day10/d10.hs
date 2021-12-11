{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Either

-- data Result = Corrupted symbol | Incomplete 

main :: IO ()
main = do
    raw <- readFile "inputT.txt"
    let parsed = lines raw
    putStrLn $ show (sum $ map (errValue) (part1 parsed))
    putStrLn $ show (part2 parsed)

part1 [] = []
part1 (l:ls) = parseLine l:(part1 ls)

part2 parsed =
    let uncorrupted = filter (\l -> isRight $ parseLine l) parsed
    in uncorrupted

-- complete :: [String] -> [String]
-- complete [] = []
-- complete (l:ls) = complete ls ++ [(complete' (opp (head l)) "" l)]

-- complete' _ ends [] = ends
-- complete' ends (x:xs) =
--     case consumeUntilEndChar2 (opp x) (xs ++ ends) of
--         Left e -> complete' ((opp x):ends) (x:xs)
--         Right r -> complete' ends r

parseLine [] = Right ()
parseLine line =
    case parseChunk line of
        Left err -> Left err
        Right remainder -> parseLine remainder

parseChunk ('(':xs) = consumeUntilEndChar ')' xs
parseChunk ('[':xs) = consumeUntilEndChar ']' xs
parseChunk ('{':xs) = consumeUntilEndChar '}' xs
parseChunk ('<':xs) = consumeUntilEndChar '>' xs
parseChunk a = error a
opp '(' = ')'
opp '[' = ']'
opp '{' = '}'
opp '<' = '>'
opp ')' = '('
opp ']' = '['
opp '}' = '{'
opp '>' = '<'
-- opp a = error (show a)

isStartChar '(' = True
isStartChar '[' = True
isStartChar '{' = True
isStartChar '<' = True
isStartChar _ = False

isEndChar ')' = True
isEndChar ']' = True
isEndChar '}' = True
isEndChar '>' = True
isEndChar _ = False

errValue (Left ')') = 3
errValue (Left ']') = 57
errValue (Left '}') = 1197
errValue (Left '>') = 25137
errValue _ = 0

consumeUntilEndChar goal (x:xs) =
    if isStartChar x then
        case parseChunk (x:xs) of
            Left e -> Left e
            Right r -> consumeUntilEndChar goal r
    else if isEndChar x && x == goal then
        Right xs
    else if isEndChar x && x /= goal then
        -- Corrupted
        Left x
    else consumeUntilEndChar goal xs

-- Incomplete
consumeUntilEndChar goal [] = Right ""
