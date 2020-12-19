{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Text.ParserCombinators.ReadP
import Utils

data Rule = Char Char | Seq [Int] | Or [Int] [Int]

parseSpacedInts :: ReadP [Int]
parseSpacedInts = parseInt `sepBy1` char ' '

parseChar, parseSeq, parseOr :: ReadP Rule
parseChar = Char <$> between (char '"') (char '"') get
parseSeq = Seq <$> parseSpacedInts
parseOr = Or
    <$> parseSpacedInts
    <*  string " | "
    <*> parseSpacedInts

newtype RuleDecl = Decl { getDecl :: (Int, Rule) }

parseDecl :: ReadP (Int, Rule)
parseDecl = (,)
    <$> parseInt
    <*  string ": "
    <*> (parseChar +++ parseSeq +++ parseOr)

instance Read RuleDecl where
    readsPrec _ = readP_to_S (Decl <$> parseDecl)

buildParser :: M.Map Int Rule -> Int -> ReadP String
buildParser declMap idx = case declMap M.! idx of
    Char c -> string [c]
    Seq is -> allParsers is
    Or is js -> allParsers is +++ allParsers js
    where allParsers [] = return []
          allParsers (i:is) = liftA2 (++) (buildParser declMap i) (allParsers is)

main :: IO ()
main = do
    input <- TI.readFile "input.txt"
    let [rulesStr, messages] = map T.unpack . T.splitOn "\n\n" $ input
        declMap = M.unions . map (uncurry M.singleton . getDecl . read) . lines $ rulesStr
        declMap' = M.fromList [(8, Or [42] [42, 8]), (11, Or [42, 31] [42, 11, 31])] <> declMap
        root  = buildParser declMap  0
        root' = buildParser declMap' 0
        isValidParse p s = not $ null [x | (x, "") <- readP_to_S p s]
    print . countPred (isValidParse root ) . lines $ messages
    print . countPred (isValidParse root') . lines $ messages