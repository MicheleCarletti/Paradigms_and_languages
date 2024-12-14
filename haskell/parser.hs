-- Parser for math expressions based on Abstract Syntax Tree (AST)

data Expr = Add Expr Expr   -- Sum
    | Sub Expr Expr -- Subtraction
    | Mul Expr Expr -- Multiplication
    | Div Expr Expr -- Division
    | Pow Expr Expr -- Power
    | Val Double    -- Numeric value
    deriving (Show)

-- Hierarchical parser: values, terms (*,/,^) and expressions (+,-)
-- Extracts values or parenthesis
parseValue :: String -> (Expr, String)
parseValue ('-':xs) =
    let (Val num, rest) = parseValue xs -- Support for negative numbers
    in (Val (-num), rest)
parseValue ('(':xs) =
    let (expr, rest) = parseExpr xs
    in case rest of
        (')':rest') -> (expr, rest')    -- Parenthesis close
        _ -> error "Error: missing ')' "
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

-- Parsing function
parse :: String -> Expr
parse input =
    let (expr, rest) = parseExpr (filter (/=' ') input) -- Remove blank spaces
    in if null rest 
        then expr
        else error ("Error: missing values " ++ rest)

-- Evaluate AST
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


main::IO ()
main = do
    putStrLn "Insert a math expression, ex. 3 + 5 * (2 - 8)"
    input <- getLine
    let ast = parse input
    putStrLn "AST: "
    print ast
    putStrLn "Result:"
    print(eval ast)   