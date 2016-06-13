import Cam
import Lambda
import Converter

import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

data Flag
    = Lambda                -- -l
    | CamCode               -- -c
    | TestCase              -- -t
    | OnlyResult            -- -o
    | Fact                  -- -f
    | Fib                   -- -F
    | Help                  -- --help
    deriving (Eq,Ord,Enum,Show,Bounded)

flags =
    [Option ['l'] []       (NoArg Lambda)
         "Interpret input as lambda code"
    ,Option ['c'] []       (NoArg CamCode)
         "Interpret input as CAM code"
    ,Option ['t'] []       (NoArg TestCase)
         "Use test case from library"
    ,Option ['o'] []       (NoArg OnlyResult)
         "Print only result without steps"
    ,Option ['f'] []       (NoArg Fact)
         "Calculate factorial"
    ,Option ['F'] []       (NoArg Fib)
         "Calculate Fibonacci"
    ,Option []    ["help"] (NoArg Help)
         "Print this help message"
    ]

set f = [f]

testCases = [
        (CamCode, "0", "< Λ(Snd+), < '4, '3 >> ε"),
        (CamCode, "1", "<Λ(<Snd, <'4, '3>>ε),Λ(Snd+)>ε"),
        (CamCode, "2", "<< L(L(< Fst Snd , Snd > e)) , L(< L(Snd +) , < '1 , Snd > > e) > e , '3 > e"),
        (CamCode, "3", "< < L(L(< L(Snd P) , < Fst Snd , Snd > > e)) , 'a > e , 'b > e"),
        (CamCode, "4", "< 'True branch (('1), ('2))"),
        (CamCode, "5", "< <'1,'1>= branch (('1), ('2))"),
        (CamCode, "6", "<<Y(if<Snd,'0>=br(('1),(<Snd,<FstSnd,<Snd,'1>->ε>*)))>Λ(if<Snd,'0>=br(('1),(<Snd,<FstSnd,<Snd,'1>->ε>*)))><Snd,'3>ε"),
        (CamCode, "7", "<Λ(<Snd, '5>ε), <Λ(Λ(if<Λ(Snd =), <Snd, '0>>εbr(('1),(if<Λ(Snd =), <Snd, '1>>εbr(('1),(<Λ(Snd +), <<FstSnd, <Λ(Snd -), <Snd, '1>>ε>ε, <FstSnd, <Λ(Snd -), <Snd, '2>>ε>ε>>ε)))))), Y(if<Λ(Snd =), <Snd, '0>>εbr(('1),(if<Λ(Snd =), <Snd, '1>>εbr(('1),(<Λ(Snd +), <<FstSnd, <Λ(Snd -), <Snd, '1>>ε>ε, <FstSnd, <Λ(Snd -), <Snd, '2>>ε>ε>>ε)))))>ε>ε"),

        (Lambda, "8", "(λx.λy.+[x, y]) 1 2"),
        (Lambda, "9", "(λg.g 3)(Y(λf.λn.if(=[n,0])then(1)else(*[n,f(-[n,1])])))")
    ]

listTestCases = foldl (\res (t, num, input) -> res ++ num ++ ") " ++ input ++ "\n") "Test case library:\n" testCases

parseArgs argv =
    case getOpt Permute flags argv of
        (args,fs,[]) -> do
            let files = if null fs then ["-"] else fs
            if Help `elem` args
                then do hPutStrLn stderr (usageInfo header flags)
                        exitWith ExitSuccess
                else return (nub (concatMap set args), files)
        (_,_,errs)      -> do
            hPutStrLn stderr (concat errs ++ usageInfo header flags)
            exitWith (ExitFailure 1)
        where header = "Usage: cam [-lcto] [lambda term/cam code/number from library]\n" ++ listTestCases

getCamCode flags input = do
    if Lambda `elem` flags then do
        let lambdaTerm = fst $ head $ Lambda.parse input
        putStrLn "Processig input as lambda term"
        putStrLn $ "Term after parsing: " ++ (show lambdaTerm)
        putStrLn "Converting lambda-term to CAM code"
        return $ convertLambdaToCamCode lambdaTerm
    else if CamCode `elem` flags then do
        return input
    else if TestCase `elem` flags then do
        let (t, num, testCase) = (testCases !! (read input :: Int))
        if t  == Lambda then
            getCamCode [Lambda] $ testCase
        else
            return $ testCase
    else
        return ""

processCamCode flags camCode =
    if OnlyResult `elem` flags then
        evalCamCodeSilent camCode
    else
        evalCamCode camCode

processFactorial flags n =
    if OnlyResult `elem` flags then
        (show n) ++ "! = " ++ (calcFact True n)
    else
        (show n) ++ "! = " ++ (calcFact False n)

processFibonacci flags n =
    if OnlyResult `elem` flags then
        "Fibonacci " ++ (show n) ++ " = " ++ (calcFib True n)
    else
        "Fibonacci " ++ (show n) ++ " = " ++ (calcFib False n)

processInput :: [Flag] -> String -> IO()
processInput flags input = do
    putStrLn $ "Processing input: " ++ input
    if Fact `elem` flags then
        putStrLn $ processFactorial flags (read input :: Integer)
    else if Fib `elem` flags then
        putStrLn $ processFibonacci flags (read input :: Integer)
    else do
        camCode <- getCamCode flags input
        putStrLn $ "CAM code:" ++ camCode
        putStrLn "Passing CAM code to CAM interpreter"
        putStrLn $ processCamCode flags camCode

main = do
    (as, input) <- getArgs >>= parseArgs
    putStrLn $ "Flags: " ++ show as
    putStrLn $ "Input: " ++ show input
    mapM_ (\inp -> processInput as inp) input
