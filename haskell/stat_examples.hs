import Data.List (group, sort, maximumBy)
import Data.Ord (comparing)

average :: (Fractional a) => [a] -> Maybe a
average [] = Nothing
average xs = Just (sum(xs) / fromIntegral (length xs))

median :: (Ord a, Fractional a) => [a] -> Maybe a
median [] = Nothing
median xs =
    let sorted = sort xs
        n = length sorted
    in if odd n
        then Just (sorted !! (div n 2)) -- if odd it's the central element
        else Just ((sorted !! (div n 2 -1) + sorted !! (div n 2))/2)   -- Average of central values

mode :: (Ord a) => [a] -> Maybe [a]
mode [] = Nothing
mode xs =
    let sorted = sort xs
        grouped = group sorted  -- Group equal elements [1,1,4,6] -> [[1,1],[4],[6]]
        frequencies = map (\g -> (head g, length g)) grouped    -- Count instances  [[1,1],[4],[6]] -> [(1,2),(4,1),(6,1)]
        maxFreq = maximumBy (comparing snd) frequencies -- Find element with max instances
        maxCount = snd maxFreq  -- max frequence
        modes = [x | (x, count) <- frequencies, count == maxCount]  -- All elements with max freq
    in Just modes

main :: IO ()
main = do
    putStrLn "Insert a list of numeric values"
    input <- getLine
    let numbers = read input :: [Double]
    case average numbers of
        Nothing -> putStrLn "Invalid input. Empty list!"
        Just r -> putStrLn $ "Average = " ++ show r
    case median numbers of
        Nothing -> putStrLn "Invalid input. Empty list!"
        Just r -> putStrLn $ "Median = " ++ show r
    case mode numbers of
        Nothing -> putStrLn "Invalid input. Empty list!"
        Just r -> putStrLn $ "Mode = " ++ show r
        
    
