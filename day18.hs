import Text.ParserCombinators.ReadP
import Utils

data Exp = Add Exp Exp | Mul Exp Exp | Number Int

eval :: Exp -> Int
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r
eval (Number n) = n


operator :: Char -> ReadP Char
operator = between skipSpaces skipSpaces . char

parens :: ReadP a -> ReadP a
parens = between (char '(') (char ')')


parseAdd, parseMul :: ReadP (Exp -> Exp -> Exp)
parseAdd = Add <$ operator '+'
parseMul = Mul <$ operator '*'

parseNum :: ReadP Exp
parseNum = Number <$> parseInt

parseExp :: ReadP Exp
parseExp = chainl1 (parseNum +++ parens parseExp) (parseAdd +++ parseMul)

instance Read Exp where
    readsPrec _ = readP_to_S parseExp


newtype Exp' = Exp' { getExp :: Exp }

parseExp' :: ReadP Exp
parseExp' = chainl1 (chainl1 (parseNum +++ parens parseExp') parseAdd) parseMul

instance Read Exp' where
    readsPrec _ = readP_to_S (Exp' <$> parseExp')


main :: IO ()
main = do
    input <- readFile "input.txt"
    let exps  = map read . lines $ input -- read magic hiding that these lines are parsing two different things
        exps' = map read . lines $ input
    print . sum . map  eval           $ exps
    print . sum . map (eval . getExp) $ exps'