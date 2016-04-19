import System.IO

data 

data Token =
    TPush
    |TSwap
    |TCons
    |TCar
    |TCdr
    |TApp
    |TCur
    |TQuote
    |TOpenParentheses
    |TCloseParentheses
    |TIdentifier String deriving (Show)

tokenize :: String -> [Token]
tokenize string =
    let
        parseId acc (x:'F':'s':'t':xs) = (acc, (x:'F':'s':'t':xs))
        parseId acc (x:'S':'n':'d':xs) = (acc, (x:'S':'n':'d':xs))
        parseId acc (x:xs)
            | x `elem` ['<', '>', ',', '(', ')', '@', 'L', ' ', '\'', '|'] = (acc, (x:xs))
            | otherwise = parseId (acc ++ [x]) xs

        tokenize' [] tokens = tokens
        tokenize' ('<':xs) tokens = tokenize' xs (TPush:tokens)
        tokenize' (',':xs) tokens = tokenize' xs (TSwap:tokens)
        tokenize' ('>':xs) tokens = tokenize' xs (TCons:tokens)
        tokenize' ('F':'s':'t':xs) tokens = tokenize' xs (TCar:tokens)
        tokenize' ('S':'n':'d':xs) tokens = tokenize' xs (TCdr:tokens)
        tokenize' ('@':xs) tokens = tokenize' xs (TApp:tokens)
        tokenize' ('ε':xs) tokens = tokenize' xs (TApp:tokens)
        tokenize' ('L':xs) tokens = tokenize' xs (TCur:tokens)
        tokenize' ('Λ':xs) tokens = tokenize' xs (TCur:tokens)
        tokenize' ('\'':xs) tokens = tokenize' xs (TQuote:tokens)
        tokenize' ('(':xs) tokens = tokenize' xs (TOpenParentheses:tokens)
        tokenize' (')':xs) tokens = tokenize' xs (TCloseParentheses:tokens)
        tokenize' (' ':xs) tokens = tokenize' xs tokens
        tokenize' ('|':xs) tokens = tokenize' xs tokens
        tokenize' (xs) tokens =
            let (n, xss) = parseId [] xs
            in tokenize' xss ((TIdentifier n):tokens)
    in
        reverse $ tokenize' string []

test = "< Λ(Snd+), < '4, '3 >> ε"

main = do
    putStrLn "CAM"
