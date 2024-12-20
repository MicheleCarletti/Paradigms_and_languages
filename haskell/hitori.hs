import Data.Array
import Data.List

type Board = Array (Int, Int) Int

boardH :: Int
boardH = 8

boardW :: Int
boardW = 8

-- Move all array's elemnts in a list of lists
arrayToList :: Board -> [[Int]]
arrayToList a =
    [[a ! (r,c) | c <- [1..boardW]] | r <- [1..boardH]] 
---------------------------------------------------------------------------------------------------------------------------------------------------
-- 1 Rule: no adjacent shadowed cells

-- Check ther're no adjacent shadowed cells
checkNoAdjB :: [Int] -> Bool
checkNoAdjB [] = True
checkNoAdjB [x] = True
checkNoAdjB (x:y:xs) 
    | x == y = False
    | otherwise = checkNoAdjB (y:xs)

checkNABinR :: [[Int]] -> Bool
checkNABinR [] = True
checkNABinR grid = all checkNoAdjB grid 

checkNABinC :: [[Int]] -> Bool
checkNABinC [] = True
checkNABinC grid = all checkNoAdjB (transpose grid) 

checkFirtsRule :: Board -> Bool
checkFirtsRule b = 
    let grid = arrayToList b
    in checkNABinC grid && checkNABinR grid 
---------------------------------------------------------------------------------------------------------------------------------------------------
-- 2 Rule: no duplicated elements in each row / cell

checkNoDuplicated :: [Int] -> Bool
checkNoDuplicated [] = True
checkNoDuplicated (x:xs) 
    | x /= 0 && elem x xs = False   -- There's a duplicated element
    | otherwise = checkNoDuplicated xs  -- Go forward checking the list

checkNDinR :: [[Int]] -> Bool
checkNDinR [] = True
checkNDinR grid = all checkNoDuplicated grid

checkNDinC :: [[Int]] -> Bool
checkNDinC [] = True
checkNDinC grid = all checkNoDuplicated (transpose grid)

checkSecondRule :: Board -> Bool
checkSecondRule b = 
    let grid = arrayToList b
    in checkNDinC grid && checkNDinR grid
---------------------------------------------------------------------------------------------------------------------------------------------------
-- 3 Rule: non-shadowed cells must be connected

-- Flooding to check if the unshadowed (/= 0) elements are connected
fill :: Board -> (Int, Int) -> Board
fill board (x,y) = floodFill board [(x,y)]
    where
        -- Flood fill with list of cells
        floodFill :: Board -> [(Int, Int)] -> Board
        floodFill b [] = b
        floodFill b (pos:rest)
            | not (inBounds pos) = floodFill b rest   -- Out board, ignore
            | b ! pos == 0 = floodFill b rest -- Already checked or shadowed cell, go forward
            | otherwise = 
                let b' = b // [(pos, 0)]    -- Mark the cell with 0
                    neighbors = directions pos
                in floodFill b' (rest ++ neighbors)
        
        -- Get adj cells
        directions :: (Int, Int) -> [(Int, Int)]
        directions (i,j) = [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]

        -- Check position within the board
        inBounds :: (Int, Int) -> Bool
        inBounds (r, c) = 1 <= r && r <= boardH && 1 <= c && c <= boardW 

-- Find the first cell in the board /= 0
firstNonZero :: Board -> Maybe (Int, Int)
firstNonZero b = 
    case filter (\pos -> b ! pos /= 0) (indices b) of
        [] -> Nothing
        (x:_) -> Just x

checkRules :: Board -> Bool
checkRules b = 
    let
        firstRes = checkFirtsRule b
        secondRes = checkSecondRule b
        thirdRes = case firstNonZero b of
            Nothing -> False
            Just start -> do
                let r = fill b start
                if all (==0) (elems r) then True else False
    in firstRes && secondRes && thirdRes    

-- Main function
main :: IO ()
main = do
    -- Read the board from a file
    content <- readFile "hitori.txt"
    let rows = lines content
        boardList = concatMap (map read . words) rows :: [Int]
    
    -- Create the board as a 2D array
    let board = array ((1, 1), (boardH, boardW)) 
                      [((i, j), boardList !! ((i-1) * boardW + (j-1))) | i <- [1..boardH], j <- [1..boardW]]
    printBoard board

    case checkRules board of
        True -> putStrLn "Well done!"
        False -> putStrLn "The board is not valid"

  where
    -- Function to print the board
    printBoard b = mapM_ (\i -> putStrLn $ unwords [show (b ! (i, j)) | j <- [1..boardH]]) [1..boardW]
