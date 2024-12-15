import System.Random
import Text.Read (readMaybe)

check :: Int -> Int -> String
check guess real 
    | guess == real =  "Congratulation, you're right!"
    | otherwise =  "Nope, it was " ++ show real

main :: IO ()
main = do
    putStrLn "Guess a number between 0 and 10. q to quit"
    input <- getLine
    if input /= "q" then do
        gen <- newStdGen
        let (real, _) = randomR(0,10) gen
        case readMaybe input :: Maybe Int of    -- Parse the input as Maybe Int to chacth invalid input
            Just guess -> do
                if guess > 10 || guess < 0 then do
                    putStrLn "The value should be between 0 and 10"
                    main
                else do
                    let result = check guess real
                    putStrLn result
                    main
            Nothing -> do
                putStrLn "Invalid input"
                main    
    else
        putStrLn "Goodbye"
