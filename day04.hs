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
validHeight (Height value unit) = (unit == "cm" && value >= 150 && value <= 193) ||
                                  (unit == "in" && value >= 56  && value <= 76 )

    
hasAllFields :: Passport -> Bool
hasAllFields p = all (`member` p) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValid :: Passport -> Bool
isValid p = foldr1 (&&)
    [
        hasAllFields p,
        let byr = read (p ! "byr")
         in byr >= 1920 && byr <= 2002,
        let iyr = read (p ! "iyr")
         in iyr >= 2010 && iyr <= 2020,
        let eyr = read (p ! "eyr")
         in eyr >= 2020 && eyr <= 2030,
        let hgt = readMaybe (p ! "hgt")
         in maybe False validHeight hgt,
        let (first:rest) = p ! "hcl"
         in first == '#' && length rest == 6 && all isHexDigit rest,
        let ecl = p ! "ecl"
         in elem ecl ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
        let pid = p ! "pid"
         in length pid == 9 && all isDigit pid
    ]

main :: IO ()
main = do
    input <- readFile "input.txt"
    let All passports = read input
    print $ countPred hasAllFields passports
    print $ countPred isValid passports