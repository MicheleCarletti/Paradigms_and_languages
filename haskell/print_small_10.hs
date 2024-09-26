main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input  -- Given lines it return a list with many elements as lines
        shortLines = filter (\lines -> length lines < 10) allLines   -- Filter the list with only elements < 10
        result = unlines shortLines -- From list back to lines
    in result