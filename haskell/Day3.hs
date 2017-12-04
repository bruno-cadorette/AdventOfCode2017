module Day3 where

import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.Map as Map

input = 361527

up    (a, b) = (a, b + 1)
down  (a, b) = (a, b - 1)
left  (a, b) = (a - 1, b)
right (a, b) = (a + 1, b)
directions = [right, up, left, down]

day3Gen = scanl (\c f -> f c) (0,0) $ concat $ zipWith replicate steps (cycle directions)
    where
        steps = concatMap (replicate 2) [1..]
getValue :: Num a => (Integer, Integer) -> Map.Map (Integer, Integer) a -> a
getValue position table = sum $ mapMaybe (\f -> Map.lookup (f position) table) dir
    where 
        dir = directions ++ liftA2 (.) [up, down] [left, right]

setValue table coord = 
    let x = getValue coord table
    in (Map.insert coord x table, x)

day3Part1 = day3Gen !! (input - 1)
day3Part2 = find (> input) $ snd $ mapAccumL setValue (Map.singleton (0,0) 1) $ drop 1 day3Gen