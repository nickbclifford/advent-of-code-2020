import Data.Char
import Data.Map.Strict
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)
import Utils hiding (count)

type Field = String
type Passport = Map Field String
newtype AllPassports = All [Passport]

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

parseSingleField = singleton <$> count 3 get <* char ':' <*> munch1 (\c -> c == '#' || isAlphaNum c)
parsePassport = mconcat <$> parseSingleField `sepBy1` (char ' ' +++ char '\n')
parsePassports = All <$> parsePassport `sepBy1` string "\n\n"

instance Read AllPassports where
    readsPrec _ = readP_to_S parsePassports

data Height = Height Int String

parseHeight = Height <$> (read <$> munch1 isDigit) <*> count 2 get

instance Read Height where
    readsPrec _ = readP_to_S parseHeight

hasAllFields :: Passport -> Bool
hasAllFields p = all (`member` p) requiredFields

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
        let mHeight = readMaybe (p ! "hgt")
            validHeight (Height n unit) = (unit == "cm" && n >= 150 && n <= 193) || (unit == "in" && n >= 56 && n <= 76)
        in maybe False validHeight mHeight,
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