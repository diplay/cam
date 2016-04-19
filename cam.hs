import System.IO

data Command =
    Push
    |Swap
    |Cons
    |Cur Code
    |Car
    |Cdr
    |App
    |Identifier String
    |Quote Command
    |Error

instance Show Command where
    show = printCommand

data Code = Code [Command]

instance Show Code where
  show = stringify
instance Read Code where
  readsPrec _ x = parse x

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

data Term = Empty
    |Term Code
    |Pair Term Term
    |Semicolon Term Term

instance Show Term where
  show = printTerm

data State = State Term Code Term

instance Show State where
    show = printState

printState (State t c s) = (show t) ++ " | " ++ (show c) ++ " | " ++ (show s)

printCommand Push = "<"
printCommand Swap = ","
printCommand Cons = ">"
printCommand (Cur c) = "Λ(" ++ (show c) ++ ")"
printCommand Car = "Fst"
printCommand Cdr = "Snd"
printCommand App = "ε"
printCommand (Quote c) = "'" ++ (show c)
printCommand (Identifier s) = show s
printCommand Error = "Error"

printTerm Empty = "[]"
printTerm (Term code) = show code
printTerm (Pair t1 t2) = "[" ++ (show t1) ++ ", " ++ (show t2) ++ "]"
printTerm (Semicolon t1 t2) = (show t1) ++ " : " ++ (show t2)

stringify (Code []) = ""
stringify (Code (c:cs)) = (show c) ++ " " ++ show (Code cs)

push (State t (Code (c:cc)) s) = State t (Code cc) (Pair t s)
swap (State t (Code (c:cc)) (Pair s ss)) = State s (Code cc) (Pair t ss)
swap (State t (Code (c:cc)) s) = State s (Code cc) t
cons (State t (Code (c:cc)) (Pair s ss)) = State (Pair s t) (Code cc) ss
cur (State t (Code ((Cur cc):cc1)) (Pair s ss)) = State (Pair (Semicolon (Term cc) s) t) (Code cc1) ss
car (State (Pair s t) (Code (c:cc)) ss) = State s (Code cc) ss
cdr (State (Pair s t) (Code (c:cc)) ss) = State t (Code cc) ss
app (State (Pair (Semicolon (Term (Code cc)) s) t) (Code (App:cc1)) ss) = State (Pair s t) (Code (cc ++ cc1)) ss
quote (State t (Code ((Quote c):cc)) s) = State (Term (Code [c])) (Code cc) s
execId (State t (Code ((Identifier _):cc)) s) = (State t (Code [Error]) s)

doStep (State t (Code (Push:cc)) s) = push (State t (Code (Push:cc)) s)
doStep (State t (Code (Swap:cc)) s) = swap (State t (Code (Swap:cc)) s)
doStep (State t (Code (Cons:cc)) s) = cons (State t (Code (Cons:cc)) s)
doStep (State t (Code ((Cur code):cc)) s) = cur (State t (Code ((Cur code):cc)) s)
doStep (State t (Code (Car:cc)) s) = car (State t (Code (Car:cc)) s)
doStep (State t (Code (Cdr:cc)) s) = cdr (State t (Code (Cdr:cc)) s)
doStep (State t (Code (App:cc)) s) = app (State t (Code (App:cc)) s)
doStep (State t (Code ((Quote c):cc)) s) = quote (State t (Code ((Quote c):cc)) s)
doStep (State t (Code [Error]) s) = State t (Code []) s
doStep (State t (Code ((Identifier id):cc)) s) = execId (State t (Code ((Identifier id):cc)) s)
doStep (State t _ s) = State t (Code [Error]) s

parse code =
    let
        parse' :: [Token] -> [Command]
        parse' [] = []
        parse' (TPush:xs) = (Push : parse' xs)
        parse' (TSwap:xs) = (Swap : parse' xs)
        parse' (TCons:xs) = (Cons : parse' xs)
        parse' (TCar:xs) = (Car : parse' xs)
        parse' (TCdr:xs) = (Cdr : parse' xs)
        parse' (TApp:xs) = (App : parse' xs)
        parse' (TQuote:t:xs) = ((Quote (head $ parse' [t])) : parse' xs)
        parse' ((TIdentifier id):xs) = ((Identifier id) : parse' xs)
        parse' (TCur:xs) =
            let
                parseCur acc (TOpenParentheses:xs) = parseCur acc xs
                parseCur acc (TCloseParentheses:xs) = (acc, xs)
                parseCur acc (x:xs) = parseCur (acc ++ (parse' [x])) xs
                (curCode, xss) = parseCur [] xs
            in
                ((Cur (Code curCode)) : parse' xss)
    in [(Code (parse' $ tokenize code), "")]


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

parseCode codeString = fst $ head $ parse codeString

test = "< Λ(Snd+), < '4, '3 >> ε"
testState = State Empty (parseCode test) Empty

doAllSteps accStates (State t (Code []) s) =
    (State t (Code []) s):accStates
doAllSteps accStates initState =
    doAllSteps (initState:accStates) (doStep initState)

testPrint = do
    let testResult = doAllSteps [] testState
    putStrLn $ "Test case:" ++ test
    putStrLn $ "Test case after parse:" ++ (show $ parseCode test)
    mapM_ (putStrLn . show) $ reverse testResult
