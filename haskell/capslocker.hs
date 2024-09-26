import Control.Monad
import Data.Char


--main = forever $ do
    --putStr "Give me some input: "
    --l <- getLine
    --putStrLn $ map toUpper l
-- This is equivalent to use getContents
main = do
    contents <- getContents
    putStr (map toUpper contents)