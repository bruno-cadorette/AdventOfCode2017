module Day2 where

import Control.Applicative
import Data.Maybe

day2Input = parseDay2 <$> readFile "day2.txt"

parseDay2 :: String -> [[Int]]
parseDay2 = fmap (fmap read . words) . lines

solveDay2p1 :: [[Int]] -> Int
solveDay2p1 = sum . fmap (\ys -> maximum ys - minimum ys)

solveDay2p2 :: [[Int]] -> Maybe Int
solveDay2p2 =  fmap sum . traverse sp2
    where
        sp2 xs = fmap (\(a,b) -> max a b `div` min a b) $ listToMaybe $ filter (\(a,b) -> a `mod` b == 0 && a /= b) $ liftA2 (,) xs xs