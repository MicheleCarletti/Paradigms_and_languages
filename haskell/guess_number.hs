import System.Random
import Control.Monad
main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (secret, newGen) = randomR (1,10) gen :: (Int, StdGen)
    putStrLn "Which number (1-10) am I thinking of?"
    guess <- getLine
    when (not $ null guess) $
        if guess == show secret
        then putStrLn "You are correct!"
        else do
            putStrLn ("Sorry, it was " ++ show secret)
            askForNumber newGen