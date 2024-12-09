import System.Random

-- Define the arena and the player
arenaWidth :: Int
arenaWidth = 10

arenaHeight :: Int
arenaHeight = 10

-- Player's position within the board
type Position = (Int, Int)  -- (x,y)

-- Fixed obstacles
type Obstacles = [Position]

-- Game state
data GameState = GameState {
    playerPosition :: Position,
    obstacles :: Obstacles,
    enemyPosition :: Position   -- enemy randomly spawning in the arena
} deriving (Show)

-- Check wether a cell is valid
isValidPosition :: Position -> Obstacles -> Bool
isValidPosition pos obs = not (pos `elem` obs)

-- Generate a random position for the enemy
randomPosition :: StdGen -> Position
randomPosition gen = 
    let (x, gen') = randomR (0, arenaWidth-1) gen
        (y, _) = randomR (0, arenaHeight-1) gen'
    in (x, y)


-- Player and enemy moves
movePlayer :: String -> GameState -> IO GameState
movePlayer [] state = return state -- Blank input, keep the state
movePlayer (dir:_) (GameState (x,y) obs (ex,ey)) = do

    let newPos = case dir of 
            'w' -> (x, max 0 (y-1))   -- Move forward
            's' -> (x, min (arenaHeight-1) (y+1)) -- Move down
            'a' -> (max 0 (x-1), y)   -- Move left
            'd' -> (min (arenaWidth-1) (x+1), y)  -- Move right
            _ -> (x,y)    -- Do nothing

    -- Check if the new position is valid
    let playerPos = if isValidPosition newPos obs then newPos else (x,y)
    -- New position for the enemy
    gen <- newStdGen
    let (ex',ey') = randomPosition gen
    -- Check wheter player and enemy are on the same cell
    if playerPos == (ex',ey')
        then error "Game Over: you have been caught!"
        else return $ GameState playerPos obs (ex',ey') 
    

-- Render the arena. The player is represented by 'P', obstacles as '#', enemy as E and empty cells are marked as '.'
renderArena :: GameState -> String
renderArena (GameState (px,py) obs (ex,ey)) =
    unlines [[if (x,y) == (px,py) then 'P'  -- Player
             else if (x,y) == (ex,ey) then 'E'  -- Enemy
             else if (x,y) `elem` obs then '#'  -- Obstacle
             else '.'   -- free space
            | x <- [0..arenaWidth-1]]
            | y <- [0..arenaHeight-1]]


-- Game loop
gameLoop :: GameState -> IO ()
gameLoop state = do
    putStrLn $ renderArena state
    putStrLn "Move with W (up), A (left), S (down), D (right). Q to quit"
    input <- getLine
    if input == "q"
        then putStrLn "Goodbye!"
        else do 
            newState <- movePlayer input state
            gameLoop newState


-- Start the game
main :: IO ()
main = do
    putStrLn "Welcome to the game!"
    gen <- newStdGen
    let initialObstacles = [(3,3), (4,6), (7,2)]    -- Obstacles
    let initialEnemyPos = randomPosition gen 
    let initialState = GameState (5,5) initialObstacles initialEnemyPos -- Player in the center
    gameLoop initialState