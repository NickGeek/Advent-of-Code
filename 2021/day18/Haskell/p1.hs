{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Maybe
import Data.Aeson as JSON

data SnailfishData = Lit Integer | Nest SnailfishData deriving (Generic, Show)
instance ToJSON SnailfishData where
    toJSON (Lit a) = JSON.toJSON a
    toJSON (Nest sd)  = toJSON sd
instance FromJSON SnailfishData

main :: IO ()
main = do
    raw <- readFile "inputT.txt"
    -- let initialState = map (\num -> read (T.unpack num) :: Int) (T.splitOn "," (T.pack raw))
    let snailfish = map (\l -> JSON.decode (LB.pack l) :: Maybe [SnailfishData]) (lines raw)
    putStrLn $ show $ snailfish
    putStrLn $ show $ snailfish
