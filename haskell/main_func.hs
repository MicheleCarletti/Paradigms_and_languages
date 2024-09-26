import Data.Char

main = do
    line <- getLine
    if null line
        then return()
        else do 
            putStrLn (reverseWords line)
            main

-- Using function composition to reverse words
-- Ex: input: "Hey man" -> words: ["Hey", "man"] -> map reverse: ["yeH", "nam"] -> unwords: "yeH nam"
reverseWords :: String -> String
reverseWords = unwords . map reverse . words    