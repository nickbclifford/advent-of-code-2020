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
        bigM = product ids
        step (a, m) =
            let b  = bigM `div` m
                b' = (b ^ (m - 2)) `mod` m -- all our moduli are prime, so we can use a shortcut to find the modular inverse
            in a * b * b'
    print . uncurry (*) . minimumBy (comparing snd) . map (\i -> (i, i - rem time i)) $ ids
    -- https://crypto.stanford.edu/pbc/notes/numbertheory/crt.html#_for_several_equations
    print . (`mod` bigM) . sum . map step . filter ((/= -1) . snd) . zipWith (\w i -> (i - w, i)) [0..] $ schedule