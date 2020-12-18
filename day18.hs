import Text.ParserCombinators.ReadP
import Utils

data Exp = Add Exp Exp | Mul Exp Exp | Number Int

eval :: Exp -> Int
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r
eval (Number n) = n


operator :: Char -> ReadP Char
operator = between skipSpaces skipSpaces . char

parseAdd, parseMul :: ReadP (Exp -> Exp -> Exp)
parseAdd = Add <$ operator '+'
parseMul = Mul <$ operator '*'

parseNum :: ReadP Exp
parseNum = Number <$> parseInt

parseParens :: ReadP Exp
parseParens = between (operator '(') (operator ')') parseExp

parseExp :: ReadP Exp
parseExp = chainl1 (parseNum +++ parseParens) (parseAdd +++ parseMul)

instance Read Exp where
    readsPrec _ = readP_to_S parseExp


newtype Exp' = Exp' { getExp :: Exp }

parseParens' :: ReadP Exp
parseParens' = between (operator '(') (operator ')') parseExp'

parseExp' :: ReadP Exp
parseExp' = chainl1 (chainl1 (parseNum +++ parseParens') parseAdd) parseMul

instance Read Exp' where
    readsPrec _ = readP_to_S (Exp' <$> parseExp')



main :: IO ()
main = do
    input <- readFile "input.txt"
    let exps  = map read . lines $ input
        exps' = map read . lines $ input
    print . sum . map eval $ exps
    print . sum . map (eval . getExp) $ exps'