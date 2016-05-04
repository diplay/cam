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
    |Branch Code Code
    |Rec Code
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
    |TBranch
    |TY
    |TIdentifier String deriving (Show)

data Term = Empty
    |Term Code
    |Pair Term Term
    |Colon Term Term

data Stack = Stack [Term]

instance Show Term where
  show = printTerm

data RecMem = RecMem [(String, Term)]

data State = State Term Code Stack RecMem

instance Show State where
    show = printState

instance Show Stack where
    show = printStack

instance Show RecMem where
    show = printRecMem

--prints
printState (State t c s r) = printf "%-60s\t | %-60s\t | %s" (show t) (show c) (show s)

printCommand Push = "<"
printCommand Swap = ","
printCommand Cons = ">"
printCommand (Cur c) = "Λ(" ++ (show c) ++ ")"
printCommand Car = "Fst"
printCommand Cdr = "Snd"
printCommand App = "ε"
printCommand (Quote c) = "'" ++ (show c)
printCommand (Identifier "True") = "True "
printCommand (Identifier "False") = "False "
printCommand (Identifier s) = s
printCommand (Branch c1 c2) = "branch(" ++ (show c1) ++ ", " ++ (show c2) ++ ")"
printCommand (Rec c) = "Y(" ++ (show c) ++ ")"
printCommand Error = "Error"

printTerm Empty = "()"
printTerm (Term code) = show code
printTerm (Pair t1 t2) = "[" ++ (show t1) ++ ", " ++ (show t2) ++ "]"
printTerm (Colon t1 t2) = (show t1) ++ " : " ++ (show t2)

printStack (Stack s) = show s

printRecMem (RecMem r) = show r

stringify (Code []) = ""
stringify (Code (c:cs)) = (show c) ++ show (Code cs)

--CAM commands
push (State t (Code (c:cc)) (Stack s) r) = State t (Code cc) (Stack (t:s)) r
swap (State t (Code (c:cc)) (Stack (s:ss)) r) = State s (Code cc) (Stack (t:ss)) r
cons (State t (Code (c:cc)) (Stack (s:ss)) r) = State (Pair s t) (Code cc) (Stack ss) r
cur (State t (Code ((Cur cc):cc1)) ss r) = State (Colon (Term cc) t) (Code cc1) ss r
car (State (Pair s t) (Code (c:cc)) ss r) = State s (Code cc) ss r
cdr (State (Pair s t) (Code (c:cc)) ss r) = State t (Code cc) ss r
app (State (Pair (Colon (Term (Code cc)) s) t) (Code (App:cc1)) ss r) = State (Pair s t) (Code (cc ++ cc1)) ss r
app (State (Pair (Term (Code [Identifier recId])) t) c ss (RecMem r)) =
    let
        recBody = lookup recId r
        getNewTerm (Just t) = t
        getNewTerm Nothing = Term (Code [Error])
    in
        State (Pair (getNewTerm recBody) t) c ss (RecMem r)
app (State t c s r) = State t (Code [Error]) s r
quote (State t (Code ((Quote c):cc)) s r) = State (Term (Code [c])) (Code cc) s r
branch (State (Term (Code [Identifier "True"])) (Code ((Branch (Code cT) (Code cF)):cc)) (Stack (s:ss)) r) = State (s) (Code (cT ++ cc)) (Stack ss) r
branch (State (Term (Code [Identifier "False"])) (Code ((Branch (Code cT) (Code cF)):cc)) (Stack (s:ss)) r) = State (s) (Code (cF ++ cc)) (Stack ss) r
rec (State t (Code ((Rec c):cc)) s (RecMem r)) =
    let
        nextRecId = "v" ++ (show $ length r)
        v = Term (Code [Identifier nextRecId])
        newR = (nextRecId, Colon (Term c) (Pair t v)):r
    in
        State v (Code cc) s (RecMem newR)

--built-in operations
execId (State (Pair (Term (Code [Identifier lhs])) (Term (Code [Identifier rhs]))) (Code ((Identifier "+"):cc)) s r) = (State (Term (Code [Identifier (show $ (read lhs :: Integer) +  (read rhs :: Integer))])) (Code cc) s r)
execId (State (Pair (Term (Code [Identifier lhs])) (Term (Code [Identifier rhs]))) (Code ((Identifier "-"):cc)) s r) = (State (Term (Code [Identifier (show $ (read lhs :: Integer) -  (read rhs :: Integer))])) (Code cc) s r)
execId (State (Pair (Term (Code [Identifier lhs])) (Term (Code [Identifier rhs]))) (Code ((Identifier "*"):cc)) s r) = (State (Term (Code [Identifier (show $ (read lhs :: Integer) *  (read rhs :: Integer))])) (Code cc) s r)
execId (State (Pair (Term (Code [Identifier lhs])) (Term (Code [Identifier rhs]))) (Code ((Identifier "="):cc)) s r) = (State (Term (Code [Identifier (show $ (read lhs :: Integer) == (read rhs :: Integer))])) (Code cc) s r)
execId (State t (Code ((Identifier _):cc)) s r) = (State t (Code [Error]) s r)

--machine step execution
doStep (State t (Code (Push:cc)) s r) = push (State t (Code (Push:cc)) s r)
doStep (State t (Code (Swap:cc)) s r) = swap (State t (Code (Swap:cc)) s r)
doStep (State t (Code (Cons:cc)) s r) = cons (State t (Code (Cons:cc)) s r)
doStep (State t (Code ((Cur code):cc)) s r) = cur (State t (Code ((Cur code):cc)) s r)
doStep (State t (Code (Car:cc)) s r) = car (State t (Code (Car:cc)) s r)
doStep (State t (Code (Cdr:cc)) s r) = cdr (State t (Code (Cdr:cc)) s r)
doStep (State t (Code (App:cc)) s r) = app (State t (Code (App:cc)) s r)
doStep (State t (Code ((Quote c):cc)) s r) = quote (State t (Code ((Quote c):cc)) s r)
doStep (State t (Code [Error]) s r) = State t (Code []) s r
doStep (State t (Code ((Identifier id):cc)) s r) = execId (State t (Code ((Identifier id):cc)) s r)
doStep (State t (Code ((Branch codeT codeF):cc)) s r) = branch (State t (Code ((Branch codeT codeF):cc)) s r)
doStep (State t (Code ((Rec code):cc)) s r) = rec (State t (Code ((Rec code):cc)) s r)
doStep (State t _ s r) = State t (Code [Error]) s r

--parsing
parse code =
    let
        parse' :: [Command] -> [Token] -> ([Command], [Token])
        parse' acc (TPush:xs) = parse' (Push:acc) xs
        parse' acc (TSwap:xs) = parse' (Swap:acc) xs
        parse' acc (TCons:xs) = parse' (Cons:acc) xs
        parse' acc (TCar:xs) = parse' (Car:acc) xs
        parse' acc (TCdr:xs) = parse' (Cdr:acc) xs
        parse' acc (TApp:xs) = parse' (App:acc) xs
        parse' acc (TQuote:t:xs) = parse' ((Quote (head $ fst $ parse' [] [t])):acc) xs
        parse' acc ((TIdentifier id):xs) = parse' ((Identifier id):acc) xs
        parse' acc (TOpenParentheses:xs) = parse' acc xs
        parse' acc (TCur:TOpenParentheses:xs) =
            let
                (curCode, xss) = parse' [] xs
            in
                parse' ((Cur (Code curCode)):acc) xss
        parse' acc (TY:TOpenParentheses:xs) =
            let
                (yCode, xss) = parse' [] xs
            in
                parse' ((Rec (Code yCode)):acc) xss
        parse' acc (TBranch:TOpenParentheses:xs) =
            let
                (firstCode, xss) = parse' [] xs
                (secondCode, xsss) = parse' [] (tail xss)
            in
                parse' ((Branch (Code firstCode) (Code secondCode)):acc) (tail xsss)
        parse' acc (TCloseParentheses:xs) = (reverse acc, xs)
        parse' acc rest = (reverse acc, rest)
    in [(Code (fst $ parse' [] $ tokenize code), "")]

tokenize :: String -> [Token]
tokenize string =
    let
        parseId acc ('F':'s':'t':xs) = (acc, ('F':'s':'t':xs))
        parseId acc ('S':'n':'d':xs) = (acc, ('S':'n':'d':xs))
        parseId acc ('b':'r':'a':'n':'c':'h':xs) = (acc, ('b':'r':'a':'n':'c':'h':xs))
        parseId acc ('b':'r':xs) = (acc, ('b':'r':xs))
        parseId acc (x:xs)
            | x `elem` ['<', '>', ',', '(', ')', '@', 'L', ' ', '\'', '|', 'Y'] = (acc, (x:xs))
            | otherwise = parseId (acc ++ [x]) xs

        tokenize' [] tokens = tokens
        tokenize' ('<':xs) tokens = tokenize' xs (TPush:tokens)
        tokenize' ('i':'f':xs) tokens = tokenize' xs (TPush:tokens) --compatibility with Maltsev
        tokenize' (',':xs) tokens = tokenize' xs (TSwap:tokens)
        tokenize' ('>':xs) tokens = tokenize' xs (TCons:tokens)
        tokenize' ('F':'s':'t':xs) tokens = tokenize' xs (TCar:tokens)
        tokenize' ('S':'n':'d':xs) tokens = tokenize' xs (TCdr:tokens)
        tokenize' ('b':'r':'a':'n':'c':'h':xs) tokens = tokenize' xs (TBranch:tokens)
        tokenize' ('b':'r':xs) tokens = tokenize' xs (TBranch:tokens)
        tokenize' ('Y':xs) tokens = tokenize' xs (TY:tokens)
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
--test = "<< L(L(< Fst Snd , Snd > e)) , L(< L(Snd +) , < '1 , Snd > > e) > e , '3 > e"
--test = "< < L(L(< L(Snd P) , < Fst Snd , Snd > > e)) , 'a > e , 'b > e"
--test = "< 'True branch (('1), ('2))"
--test = "< <'1,'1>= branch (('1), ('2))"
test = "<<Y(if<Snd,'0>=br(('1),(<Snd,<FstSnd,<Snd,'1>->ε>*)))>Λ(if<Snd,'0>=br(('1),(<Snd,<FstSnd,<Snd,'1>->ε>*)))><Snd,'5>ε"

testState = State Empty (parseCode test) (Stack []) (RecMem [])

doAllSteps accStates (State t (Code []) s r) =
    (State t (Code []) s r):accStates
doAllSteps accStates initState =
    doAllSteps (initState:accStates) (doStep initState)

doAllStepsWithoutMemory (State t (Code []) s r) = (State t (Code []) s r)
doAllStepsWithoutMemory curState = doAllStepsWithoutMemory (doStep curState)

calcFact n =
    let
        factString = "<<Y(if<Snd,'0>=br(('1),(<Snd,<FstSnd,<Snd,'1>->ε>*)))>Λ(if<Snd,'0>=br(('1),(<Snd,<FstSnd,<Snd,'1>->ε>*)))><Snd,'" ++ (show n) ++ ">ε"
        factInitialState = State Empty (parseCode factString) (Stack []) (RecMem [])
        (State t c s r) = doAllStepsWithoutMemory factInitialState
        getResultFromTerm (Term (Code [Identifier result])) = Just result
        getResultFromTerm _ = Nothing
    in
        getResultFromTerm t

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
    let testCaseState = State Empty (parseCode testCase) (Stack []) (RecMem [])
    let testResult = doAllSteps [] testCaseState
    putStrLn $ "Test case:" ++ testCase
    putStrLn $ "Test case tokens:" ++ (show tokens)
    putStrLn $ "Test case after parse:" ++ (show $ parseCode test)
    mapM_ (putStrLn . show) $ reverse testResult

main = enterTest
