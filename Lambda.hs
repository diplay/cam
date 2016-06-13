module Lambda(LambdaTerm(Const, Var, App, Abs, Pair, Empty), Variable(Variable), parse) where

data Variable = Variable (String, Integer) deriving (Eq) --deBruijn code

data LambdaTerm = Const String |
    Var Variable |
    App LambdaTerm LambdaTerm |
    Abs Variable LambdaTerm |
    Pair LambdaTerm LambdaTerm |
    Empty
    deriving (Eq)

data Token =
    TLambda |
    TDot |
    TComma |
    TIdentifier String |
    TOpenParentheses |
    TCloseParentheses |
    TOpenBrackets |
    TCloseBrackets
    deriving (Show)

data VariablesMap = VarMap [(String, Integer)] --for deBruijn coding

instance Show LambdaTerm where
  show = printTerm

instance Show Variable where
  show = printVariable

printVariable (Variable (id, deBr)) = id

printTerm (Const c) = c
printTerm (Var v) = show v
printTerm (App (Abs v t) r) = "(" ++ (show (Abs v t)) ++ ")" ++ "(" ++ (show r) ++ ")"
printTerm (App l (Abs v t)) = (show l) ++ " (" ++ (show (Abs v t)) ++ ")"
printTerm (App l (App rl rr)) = (show l) ++ " (" ++ (show (App rl rr)) ++ ")"
printTerm (App l r) = (show l) ++ " " ++ (show r)
printTerm (Abs v t) = "λ" ++ (show v) ++ "." ++ (show t)
printTerm (Pair l r) = "[" ++ (show l) ++ ", " ++ (show r) ++ "]"
printTerm Empty = "()"

printDeBruijnTerm (Var (Variable (id, deBr))) = "_" ++ show deBr
printDeBruijnTerm (Abs v t) = "(λ" ++ "." ++ (printDeBruijnTerm t) ++ ")"
printDeBruijnTerm (App l r) = "(" ++ (printDeBruijnTerm l) ++ " " ++ (printDeBruijnTerm r) ++ ")"
printDeBruijnTerm (Pair l r) = "[" ++ (printDeBruijnTerm l) ++ ", " ++ (printDeBruijnTerm r) ++ "]"
printDeBruijnTerm t = show t

parse :: ReadS (LambdaTerm)
parse code =
    let
        chainApplication :: LambdaTerm -> LambdaTerm -> LambdaTerm
        chainApplication Empty arg = arg
        chainApplication t arg = App t arg

        parse' :: [Token] -> LambdaTerm -> VariablesMap -> (LambdaTerm, [Token])

        parse' [] t _ = (t, [])
        parse' (TCloseParentheses:xs) t _ = (t, xs)
        parse' (TCloseBrackets:xs) t _ = (t, xs)
        parse' (TComma:xs) t _ = (t, xs)

        parse' ((TIdentifier id):xs) t (VarMap vm) =
            let
                deBruijn = lookup id vm
                getVarOrConstFromId (Just number) = Var (Variable (id, number))
                getVarOrConstFromId Nothing = Const id
                result = chainApplication t (getVarOrConstFromId deBruijn)
            in
                parse' xs result (VarMap vm)

        parse' (TOpenParentheses:xs) t vm =
            let
                (inner, xss) = parse' xs Empty vm
                result = chainApplication t inner
            in
                parse' xss result vm

        parse' (TLambda:(TIdentifier v):TDot:xs) t (VarMap vm) =
            let
                incVarMap ((id, deBr):xs) = ((id, deBr + 1):(incVarMap xs))
                incVarMap [] = []
                newVm = VarMap ((v, 0):(incVarMap vm))
                (absTerm, xss) = parse' xs Empty newVm
                result = chainApplication t (Abs (Variable (v, 0)) (absTerm))
            in
                (result, xss)

        parse' (TOpenBrackets:xs) t vm =
            let
                (first, xss) = parse' xs Empty vm
                (second, xsss) = parse' xss Empty vm
                result = chainApplication t (Pair first second)
            in
                parse' xsss result vm

        result = parse' (tokenize code) Empty (VarMap [])
    in [(fst result, show $ snd result)]

tokenize :: String -> [Token]
tokenize string =
    let
        parseId acc [] = (acc, [])
        parseId acc (x:xs)
            | x `elem` ['<', '>', '[', ']', ',', '.', '(', ')', '@', 'L', 'λ', 'Λ', ' ', '\'', '|'] = (acc, (x:xs))
            | otherwise = parseId (acc ++ [x]) xs

        tokenize' [] tokens = tokens
        tokenize' ('L':xs) tokens = tokenize' xs (TLambda:tokens)
        tokenize' ('Λ':xs) tokens = tokenize' xs (TLambda:tokens)
        tokenize' ('λ':xs) tokens = tokenize' xs (TLambda:tokens)
        tokenize' ('(':xs) tokens = tokenize' xs (TOpenParentheses:tokens)
        tokenize' (')':xs) tokens = tokenize' xs (TCloseParentheses:tokens)
        tokenize' ('[':xs) tokens = tokenize' xs (TOpenBrackets:tokens)
        tokenize' (']':xs) tokens = tokenize' xs (TCloseBrackets:tokens)
        tokenize' ('.':xs) tokens = tokenize' xs (TDot:tokens)
        tokenize' (',':xs) tokens = tokenize' xs (TComma:tokens)
        tokenize' (' ':xs) tokens = tokenize' xs tokens
        tokenize' ('|':xs) tokens = tokenize' xs tokens
        tokenize' (xs) tokens =
            let (n, xss) = parseId [] xs
            in tokenize' xss ((TIdentifier n):tokens)
    in
        reverse $ tokenize' string []

test = "(λx.x y)(S (λx.λy.[x, y]+) z w)(a b c d w)"
