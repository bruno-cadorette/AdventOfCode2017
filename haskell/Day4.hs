module Day4 where

import Data.List

day4Input = lines <$> readFile "day4.txt"


part1 = length . filter (\xs -> let xs' = words xs in nub xs' == xs') 
part2 = length . filter (\xs -> let xs' = sort <$> words xs in nub xs' == xs') 