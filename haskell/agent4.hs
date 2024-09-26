import System.Random
import Control.Monad

class (Show a) => Actor a where
    move :: String -> [a] -> a -> [a]
    rect :: a -> (Int, Int, Int, Int)  -- (x, y, w, h)

data Arena a = Arena { actors :: [a]
                     } deriving (Show)

tick :: (Actor a) => Arena a -> String -> Arena a
tick (Arena actors) keys = Arena $ concat (map (move keys actors) actors)

operateArena :: (Actor a) => Arena a -> IO ()
operateArena arena = do
    print arena
    line <- getLine
    when (line /= "q") $ operateArena (tick arena line)

checkCollision :: (Actor a) => a -> a -> Bool
checkCollision a1 a2 = (rect a1) /= (rect a2) && y2 < y1+h1 && y1 < y2+h2 && x2 < x1+w1 && x1 < x2+w2
    where
        (x1, y1, w1, h1) = rect a1
        (x2, y2, w2, h2) = rect a2

maxX = 320
maxY = 240
actorW = 20
actorH = 20

data BasicActor = Ball { x :: Int, y :: Int, dx :: Int, dy :: Int }
                | Ghost { x :: Int, y :: Int, rnd :: StdGen}
                | Turtle { x :: Int, y :: Int, dead :: Bool} deriving (Show)

collide :: BasicActor -> BasicActor -> BasicActor
collide (Ball x y dx dy) (Ball _ _ _ _) = Ball x y (-dx) (-dy) -- Change direction
collide (Ball x y dx dy) (Turtle _ _ _) = Ball x y (-dx) (-dy) -- Change direction
collide (Turtle x y _) (Ball _ _ _ _) = Turtle x y True -- Turtle dies if hitten by ball
collide a _ = a -- Nothing happens for any other collision

instance Actor BasicActor where
    rect (Ball x y _ _) = (x, y, actorW, actorH)
    rect (Ghost x y _) = (x, y, actorW, actorH)
    rect (Turtle x y _) = (x, y, actorW, actorH)

    move keys actors ball@(Ball x y dx dy) =
        let newX = (x + dx) `mod` maxX
            newY = (y + dy) `mod` maxY
            collided = filter (checkCollision ball) actors  -- Check if the ball collides with other actors
        in if null collided
           then [Ball newX newY dx dy]
           else [collide ball (head collided)]

    move _ _ ghost@(Ghost x y rnd) = 
        let (dx, rnd1) = randomR (-1, 1) rnd
            (dy, rnd2) = randomR (-1, 1) rnd1
            newX = (x + 5 * dx) `mod` maxX
            newY = (y + 5 * dy) `mod` maxY
        in [Ghost newX newY rnd2]
    
    move keys actors turtle@(Turtle x y dead)
        | dead = [turtle] -- Dead turtle does not move
        | otherwise = 
            let newX = if 'a' `elem` keys then (x-5) `mod` maxX else if 'd' `elem` keys then (x+5) `mod` maxX else x -- Move left or right
                newY = if 'w' `elem` keys then (y-5) `mod` maxY else if 's' `elem` keys then (y+5) `mod` maxY else y -- Move forward or backward 
                movedTurtle = Turtle newX newY dead
                collided = filter (checkCollision turtle) actors -- Check if the turlte colllied with a ball
            in if null collided
               then [movedTurtle] -- No collision, the turtle is alive
               else [Turtle newX newY True] -- Collision, the turtle dies



main = do
    rnd <- newStdGen
    operateArena (Arena [Ball 200 100 5 5, Ball 230 120 (-5) (-5), Ghost 100 100 rnd, Turtle 160 120 False])