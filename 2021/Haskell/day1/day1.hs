import Text.Read
import Data.Maybe
import Data.List

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let parsed = parseFile raw
    putStrLn $ show (incrCount $ parsed)
    putStrLn $ show (part2 $ parsed)

part1 = incrCount
part2 = incrCount . sumBuffer . buffer3

parseFile :: String -> [Int]
parseFile raw = catMaybes $ map (\line -> readMaybe line :: Maybe Int) (lines raw)

incrCount :: [Int] -> Int
incrCount [] = 0
incrCount (x:xs) = incrCount' 0 x xs
incrCount' count lastReading list =
    case list of [] -> count
                 (x:xs) -> if x > lastReading then incrCount' (count + 1) x xs
                                              else incrCount' count x xs
sumBuffer :: [[Int]] -> [Int]
sumBuffer = foldr (\buf acc -> (sum buf):acc) []

buffer3 :: [a] -> [[a]]
buffer3 = buffer3' []
buffer3' :: [[a]] -> [a] -> [[a]]
buffer3' acc (a:b:c:xs) = buffer3' (acc ++ [[a, b, c]]) ([b, c] ++ xs)
buffer3' acc xs = acc -- don't include partial groups
