-- Parser for math expressions based on Abstract Syntax Tree (AST)
import Data.Bits (xor)

data Expr = Add Expr Expr   -- Sum
    | Sub Expr Expr -- Subtraction
    | Mul Expr Expr -- Multiplication
    | Div Expr Expr -- Division
    | Pow Expr Expr -- Power
    | And Expr Expr -- Logic and
    | Or Expr Expr  -- Logic or
    | Xor Expr Expr  -- Logic xor
    | Not Expr  -- Login not
    | Val Double    -- Numeric value
    | BoolVal Bool  -- Boolean value
    deriving (Show)

-- Hierarchical parser: values, terms (*,/,^) and expressions (+,-)
-- Extracts values or parenthesis
parseValue :: String -> (Expr, String)
parseValue ('-':xs) =
    let (Val num, rest) = parseValue xs -- Support for negative numbers
    in (Val (-num), rest)
parseValue ('(':xs) =
    let (expr, rest) = parseLogic xs
    in case rest of
        (')':rest') -> (expr, rest')    -- Parenthesis close
        _ -> error "Error: missing ')' "
parseValue ('T':'r':'u':'e':xs) = (BoolVal True, xs)    -- Get True value
parseValue ('F':'a':'l':'s':'e':xs) = (BoolVal False, xs)   -- Get False value
parseValue xs =
    let (num, rest) = span (`elem` "0123456789.") xs    -- Split xs in (xs1, xs2) where xs1 contains the first elemnts of xs that are digits or '.'
    in if null num 
        then error "Error: numeric value expected"
        else (Val (read num), rest) -- 'read' convert the num from string to Num value

-- Parse power (^)
parsePow :: String -> (Expr, String)
parsePow input =
    let (base, rest) = parseValue input
    in parsePow' base rest
    where
        parsePow' left ('^':xs) =
            let (right, rest) = parseValue xs
            in parsePow' (Pow left right) rest
        parsePow' left right = (left, right)    -- No other operator

-- Terms parsing (*, /) 
parseTerm :: String -> (Expr, String)
parseTerm input =
    let (factor, rest) = parsePow input
    in parseTerm' factor rest 
    where
        parseTerm' left ('*':xs) =
            let (right, rest) = parsePow xs
            in parseTerm' (Mul left right) rest
        parseTerm' left ('/':xs) =
            let (right, rest) = parsePow xs
            in parseTerm' (Div left right) rest
        parseTerm' left rest = (left, rest) -- No other operator

-- Expressions parsing
parseExpr :: String -> (Expr, String)
parseExpr input =
    let (term, rest) = parseTerm input
    in parseExpr' term rest
    where
        parseExpr' left ('+':xs) =
            let (right, rest) = parseTerm xs
            in parseExpr' (Add left right) rest
        parseExpr' left ('-':xs) =
            let (right, rest) = parseTerm xs
            in parseExpr' (Sub left right) rest
        parseExpr' left rest = (left, rest) -- No other operator

-- Logical expressions parsing
parseLogic :: String -> (Expr, String)
parseLogic ('N':'o':'t':xs) =
    let (expr, rest) = parseLogic xs
    in (Not expr, rest)
parseLogic input = 
    let (expr, rest) = parseExpr input
    in parseLogic' expr rest
    where
        parseLogic' left ('&':'&':xs) =
            let (right, rest) = parseExpr xs
            in parseLogic' (And left right) rest
        parseLogic' left ('|':'|':xs) =
            let (right, rest) = parseExpr xs
            in parseLogic' (Or left right) rest
        parseLogic' left ('X':'o':'r':xs) =
            let (right, rest) = parseExpr xs
            in parseLogic' (Xor left right) rest
        parseLogic' left right = (left, right)

-- Parsing function
parse :: String -> Expr
parse input =
    let (expr, rest) = parseLogic (filter (/=' ') input) -- Remove blank spaces
    in if null rest 
        then expr
        else error ("Error: missing values " ++ rest)

-- Evaluate AST for math expr
eval :: Expr -> Double
eval (Val x) = x    -- A single value
eval (Add e1 e2) = eval e1 + eval e2    -- Sum of sub-tree results
eval (Sub e1 e2) = eval e1 - eval e2    -- Subtraction of sub-tree results
eval (Mul e1 e2) = eval e1 * eval e2    -- Multiplication of sub-tree results
eval (Div e1 e2) = 
    let denom = eval e2
    in if denom == 0
        then error "Error: zero division!"
        else eval e1 / denom    -- Division of sub-tree results
eval (Pow e1 e2) = eval e1 ** eval e2    -- Power

-- Evaluate AST for bool expr
evalBool :: Expr -> Bool
evalBool (BoolVal b) = b    -- A single value
evalBool (And e1 e2) = evalBool e1 && evalBool e2
evalBool (Or e1 e2) = evalBool e1 || evalBool e2
evalBool (Not e1) = not(evalBool e1)
evalBool (Xor e1 e2) = evalBool e1 `xor` evalBool e2
evalBool _ = error "Error: boolean operation or non-boolean value"

main::IO ()
main = do
    putStrLn "Insert a math expression, ex. 3 + 5 * (2 - 8) or bool expression, ex. True && True"
    input <- getLine
    let ast = parse input
    putStrLn "AST: "
    print ast
    putStrLn "Result:"
    case ast of
        BoolVal _ -> print(evalBool ast)
        And _ _ -> print(evalBool ast)
        Or _  _-> print(evalBool ast)
        Xor _ _ -> print(evalBool ast)
        Not _ -> print(evalBool ast)
        _ -> print(eval ast) 
      