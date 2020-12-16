module Utils where

import Control.Applicative
import Data.Char
import qualified Text.ParserCombinators.ReadP as R

countPred :: (a -> Bool) -> [a] -> Int
countPred p = length . filter p

count :: Eq a => a -> [a] -> Int
count = countPred . (==)

parseInt :: R.ReadP Int
parseInt = read <$> R.munch1 isDigit

stringSize :: Int -> R.ReadP String
stringSize n = R.count n R.get

windows :: Int -> [a] -> [[a]]
windows m xs =
    if length xs == m
    then [xs]
    else take m xs : windows m (tail xs)

data Range = Range Int Int

inRange :: Range -> Int -> Bool
inRange (Range lo hi) = liftA2 (&&) (>= lo) (<= hi)

parseRange :: R.ReadP Range
parseRange = Range
    <$> parseInt
    <*  R.char '-'
    <*> parseInt