module Main where

import Text.BracketsValidator

main = do
    test "(meow)" (Validation (State {status = True}) [])
    test "(meow]" (Validation (State {status = False}) [CSquare,ORound])
    test "}meow)" (Validation (State {status = False}) [CRound,CCurled])

test :: String -> Validation [SymbolPrimitive] -> IO ()
test expression expectation = do
        putStrLn $ "Testing " ++ (show expression) ++ " ..."
        putStrLn $ "Expectation: " ++ (show expectation)
        putStrLn $ "Result: " ++ (show result)
        case (result == expectation) of
            True -> putStrLn "Test passed."
            False -> undefined -- This is where we fail with non-zero exit status.

    where result = (parser.lexer) expression
