{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.List
import Data.Ord

main :: IO ()
main = do
    input <- readFile "input.txt"
    let [timestamp, schedS] = lines input
        time = read timestamp
        schedule = map ((\s -> if s == "x" then -1 else read s) . T.unpack) . T.splitOn "," . T.pack $ schedS
        ids = filter (/= -1) schedule
    print . uncurry (*) . minimumBy (comparing snd) . map (\i -> (i, i - rem time i)) $ ids
    print . filter ((/= -1) . snd) . zipWith (\w i -> (i - w, i)) [0..] $ schedule
    -- just plug into a Chinese Remainder Theorem calculator online because honestly I am so angry about this