import System.IO
import Text.Printf

--types
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
    |Colon Term Term

data Stack = Stack [Term]

instance Show Term where
  show = printTerm

data State = State Term Code Stack

instance Show State where
    show = printState

instance Show Stack where
    show = printStack

--prints
printState (State t c s) = printf "%-60s\t | %-60s\t | %-60s\t |" (show t) (show c) (show s)

printCommand Push = "<"
printCommand Swap = ","
printCommand Cons = ">"
printCommand (Cur c) = "Λ(" ++ (show c) ++ ")"
printCommand Car = "Fst"
printCommand Cdr = "Snd"
printCommand App = "ε"
printCommand (Quote c) = "'" ++ (show c)
printCommand (Identifier s) = s
printCommand Error = "Error"

printTerm Empty = "()"
printTerm (Term code) = show code
printTerm (Pair t1 t2) = "[" ++ (show t1) ++ ", " ++ (show t2) ++ "]"
printTerm (Colon t1 t2) = (show t1) ++ " : " ++ (show t2)

printStack (Stack s) = show s

stringify (Code []) = ""
stringify (Code (c:cs)) = (show c) ++ show (Code cs)

--CAM commands
push (State t (Code (c:cc)) (Stack s)) = State t (Code cc) (Stack (t:s))
swap (State t (Code (c:cc)) (Stack (s:ss))) = State s (Code cc) (Stack (t:ss))
cons (State t (Code (c:cc)) (Stack (s:ss))) = State (Pair s t) (Code cc) (Stack ss)
cur (State t (Code ((Cur cc):cc1)) ss) = State (Colon (Term cc) t) (Code cc1) ss
car (State (Pair s t) (Code (c:cc)) ss) = State s (Code cc) ss
cdr (State (Pair s t) (Code (c:cc)) ss) = State t (Code cc) ss
app (State (Pair (Colon (Term (Code cc)) s) t) (Code (App:cc1)) ss) = State (Pair s t) (Code (cc ++ cc1)) ss
app (State t c s) = State t (Code [Error]) s
quote (State t (Code ((Quote c):cc)) s) = State (Term (Code [c])) (Code cc) s

--built-in operations
execId (State (Pair (Term (Code [Identifier lhs])) (Term (Code [Identifier rhs]))) (Code ((Identifier "+"):cc)) s) = (State (Term (Code [Identifier (show $ (read lhs :: Int) + (read rhs :: Int))])) (Code cc) s)
execId (State (Pair (Term (Code [Identifier lhs])) (Term (Code [Identifier rhs]))) (Code ((Identifier "*"):cc)) s) = (State (Term (Code [Identifier (show $ (read lhs :: Int) * (read rhs :: Int))])) (Code cc) s)
execId (State t (Code ((Identifier _):cc)) s) = (State t (Code [Error]) s)

--machine step execution
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

--parsing
parse code =
    let
        parseCur :: [Command] -> [Token] -> (Command, [Token])
        parseCur acc (TCloseParentheses:xs) = (Cur (Code acc), xs)
        parseCur acc xs =
            let
                (curCode, xss) = parse' [] xs
            in
                parseCur (acc ++ curCode) xss

        parse' :: [Command] -> [Token] -> ([Command], [Token])
        parse' acc (TPush:xs) = parse' (Push:acc) xs
        parse' acc (TSwap:xs) = parse' (Swap:acc) xs
        parse' acc (TCons:xs) = parse' (Cons:acc) xs
        parse' acc (TCar:xs) = parse' (Car:acc) xs
        parse' acc (TCdr:xs) = parse' (Cdr:acc) xs
        parse' acc (TApp:xs) = parse' (App:acc) xs
        parse' acc (TQuote:t:xs) = parse' ((Quote (head $ fst $ parse' [] [t])):acc) xs
        parse' acc ((TIdentifier id):xs) = parse' ((Identifier id):acc) xs
        parse' acc (TCur:TOpenParentheses:xs) =
            let
                (curCommand, xss) = parseCur [] xs
            in
                parse' (curCommand:acc) xss
        parse' acc rest = (reverse acc, rest)
    in [(Code (fst $ parse' [] $ tokenize code), "")]

tokenize :: String -> [Token]
tokenize string =
    let
        parseId acc ('F':'s':'t':xs) = (acc, ('F':'s':'t':xs))
        parseId acc ('S':'n':'d':xs) = (acc, ('S':'n':'d':xs))
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
        tokenize' ('e':xs) tokens = tokenize' xs (TApp:tokens)
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

--test
--test = "< Λ(Snd+), < '4, '3 >> ε"
--test = "<Λ(<Snd, <'4, '3>>ε),Λ(Snd+)>ε"
test = "<< L(L(< Fst Snd , Snd > e)) , L(< L(Snd +) , < '1 , Snd > > e) > e , '3 > e"
testState = State Empty (parseCode test) (Stack [])

doAllSteps accStates (State t (Code []) s) =
    (State t (Code []) s):accStates
doAllSteps accStates initState =
    doAllSteps (initState:accStates) (doStep initState)

testPrint = do
    let tokens = tokenize test
    let testResult = doAllSteps [] testState
    putStrLn $ "Test case:" ++ test
    putStrLn $ "Test case tokens:" ++ (show tokens)
    putStrLn $ "Test case after parse:" ++ (show $ parseCode test)
    mapM_ (putStrLn . show) $ reverse testResult

enterTest = do
    testCase <- getLine
    let tokens = tokenize testCase
    let testCaseState = State Empty (parseCode testCase) (Stack [])
    let testResult = doAllSteps [] testCaseState
    putStrLn $ "Test case:" ++ testCase
    putStrLn $ "Test case tokens:" ++ (show tokens)
    putStrLn $ "Test case after parse:" ++ (show $ parseCode test)
    mapM_ (putStrLn . show) $ reverse testResult
