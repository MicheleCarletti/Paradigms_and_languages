import System.IO
import Data.Char

-- Read a file and print its content to the terminal
{-
main = do
    withFile "Haiku_example.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
-}
-- Read a file capiytalize its content and creates a new file
main = do
    contents <- readFile "Haiku_example.txt"
    writeFile "Haikucaps.txt" (map toUpper contents)

