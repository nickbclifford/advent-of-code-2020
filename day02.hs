import Data.Char
import Text.ParserCombinators.ReadP hiding (count)
import Utils

data Password = Password Range Char String

parsePassword :: ReadP Password
parsePassword = Password
    <$> parseRange
    <*  skipSpaces
    <*> get
    <*  char ':'
    <*  skipSpaces
    <*> munch1 isAlpha

instance Read Password where
    readsPrec _ = readP_to_S parsePassword

isValidPassword :: Password -> Bool
isValidPassword (Password range letter pass) = inRange range (count letter pass)

positionsValid :: Password -> Bool
positionsValid (Password (Range pos1 pos2) letter pass) =
    let c1 = pass !! (pos1 - 1)
        c2 = pass !! (pos2 - 1)
    in (c1 == letter) /= (c2 == letter) -- (/=) on Bools is an xor

main :: IO ()
main = do
    input <- readFile "input.txt"
    let passwords = map read (lines input)
    print $ countPred isValidPassword passwords
    print $ countPred positionsValid passwords