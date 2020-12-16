import Control.Applicative
import Data.Char
import Data.Map.Strict
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)
import Utils


type Passport = Map String String
newtype AllPassports = All [Passport]

parseSingleField :: ReadP Passport
parseSingleField = singleton -- pipeline is easier to read than liftA2 here
    <$> stringSize 3
    <*  char ':' 
    <*> munch1 (liftA2 (||) (== '#') isAlphaNum)

parsePassport :: ReadP Passport
parsePassport = mconcat <$> parseSingleField `sepBy1` (char ' ' +++ char '\n')

parsePassports :: ReadP AllPassports
parsePassports = All <$> parsePassport `sepBy1` string "\n\n"

instance Read AllPassports where
    readsPrec _ = readP_to_S parsePassports


data Height = Height Int String

parseHeight :: ReadP Height
parseHeight = liftA2 Height parseInt (stringSize 2)

instance Read Height where
    readsPrec _ = readP_to_S parseHeight

validHeight :: Height -> Bool
validHeight (Height value unit) = (unit == "cm" && inRange (Range 150 193) value) ||
                                  (unit == "in" && inRange (Range 56  76 ) value)

    
hasAllFields :: Passport -> Bool
hasAllFields p = all (`member` p) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValid :: Passport -> Bool
isValid p = and
    [
        hasAllFields p,
        inRange (Range 1920 2002) (read (p ! "byr")),
        inRange (Range 2010 2020) (read (p ! "iyr")),
        inRange (Range 2020 2030) (read (p ! "eyr")),
        maybe False validHeight (readMaybe (p ! "hgt")),
        let (first:rest) = p ! "hcl"
         in first == '#' && length rest == 6 && all isHexDigit rest,
        (p ! "ecl") `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
        let pid = p ! "pid"
         in length pid == 9 && all isDigit pid
    ]

main :: IO ()
main = do
    input <- readFile "input.txt"
    let All passports = read input
    print $ countPred hasAllFields passports
    print $ countPred isValid passports