module Converter(convertLambdaToCamCode, convertLambdaStringToCamCode) where

import Cam
import Lambda

convertLambdaToCamCode :: LambdaTerm -> String
convertLambdaToCamCode Empty = "()"
convertLambdaToCamCode (Const c) =
    if c `elem` ["+", "*", "-"] then
        "Λ(Snd" ++ c ++ ")"
    else
        "'" ++ c

convertLambdaToCamCode (App l r) = "<" ++ (convertLambdaToCamCode l) ++ ", " ++ (convertLambdaToCamCode r) ++ ">ε"
convertLambdaToCamCode (Abs v t) = "Λ(" ++ (convertLambdaToCamCode t) ++ ")"
convertLambdaToCamCode (Pair l r) = "<" ++ (convertLambdaToCamCode l) ++ ", " ++ (convertLambdaToCamCode r) ++ ">"
convertLambdaToCamCode (Var (Variable (id, deBr))) =
    let
        convertDeBruijnNumber 0 = "Snd"
        convertDeBruijnNumber n = "Fst" ++ (convertDeBruijnNumber (n - 1))
    in
        convertDeBruijnNumber deBr

convertLambdaStringToCamCode :: String -> String
convertLambdaStringToCamCode =  convertLambdaToCamCode . fst . head . Lambda.parse

test = "(λx.λy.+[x, y]) 1 2"
