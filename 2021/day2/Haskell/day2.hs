{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Maybe
import Text.Read hiding (step)

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let parsed = parseFile raw
    putStrLn $ show (part1 $ parsed)

part1 tasks =
    let
        pos = run initialPos tasks
    in
        (hPos pos) * (depth pos)

run pos [] = pos
run pos (task:xs) =
    run (step pos task) xs

parseFile :: String -> [Task]
parseFile raw = catMaybes $ map parseLine (T.lines (T.pack raw))

parseLine :: T.Text -> Maybe Task
parseLine line =
    case (T.splitOn " " line) of
        [cmd, x] ->
            (readMaybe (T.unpack x) :: Maybe Int) >>= (\x ->
                case (T.unpack cmd) of
                    "forward" -> Just $ Forward x
                    "up" -> Just $ Up x
                    "down" -> Just $ Down x
                    otherwise -> Nothing
            )
        otherwise -> Nothing

data Pos = Pos { depth :: Int
               , hPos  :: Int
               }
               deriving (Show)
initialPos = Pos { depth = 0, hPos = 0 }

data Task = Forward Int
          | Up Int
          | Down Int
          deriving (Show)

step :: Pos -> Task -> Pos
step Pos{depth, hPos} (Forward x) = Pos { depth, hPos = hPos + x }
step Pos{depth, hPos} (Up x) = Pos { hPos, depth = depth - x }
step Pos{depth, hPos} (Down x) = Pos { hPos, depth = depth + x }
