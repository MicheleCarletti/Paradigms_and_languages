import System.Random
import Control.Monad

class (Show a) => Actor a where
    move :: a -> a

data Arena a = Arena { actors :: [a]
                     } deriving (Show)

tick :: (Actor a) => Arena a -> Arena a
tick (Arena actors) = Arena (map move actors)

operateArena :: (Actor a) => Arena a -> IO ()
operateArena a = do
    print a
    line <- getLine
    when (null line) $ operateArena (tick a)

----

maxX = 320
maxY = 240

data BasicActor = Ball { x :: Int
                       , y :: Int
                       , dx :: Int
                       , dy :: Int
                       }
                | Ghost { x :: Int
                        , y :: Int
                        , rnd :: StdGen
                        } deriving (Show)

moveX :: BasicActor -> BasicActor
moveX (Ball x y dx dy)
    | 0 <= x + dx && x + dx < maxX = Ball (x + dx) y dx dy
    | otherwise                    = Ball (x - dx) y (-dx) dy
moveX (Ghost x y rnd) = Ghost x' y rnd'
    where (d, rnd') = randomR (-1,1) rnd
          x' = (x + 5 * d) `mod` maxX

moveY :: BasicActor -> BasicActor
moveY (Ball x y dx dy)
    | 0 <= y + dy && y + dy < maxY = Ball x (y + dy) dx dy
    | otherwise                    = Ball x (y - dy) dx (-dy)
moveY (Ghost x y rnd) = Ghost x y' rnd'
    where (d, rnd') = randomR (-1,1) rnd
          y' = (y + 5 * d) `mod` maxY

instance Actor BasicActor where
    move = moveX . moveY 

----

data Wall = Wall { wx :: Int
                 , wy :: Int
                 } deriving (Show)

instance Actor Wall where
    move = id    -- move w = w  The id function returns the input iteself
        
----
data ArenaActor = Basic BasicActor | Static Wall deriving (Show)

instance Actor ArenaActor where
    move (Basic actor) = Basic (move actor) -- Dynamic actors move
    move (Static wall) = Static wall  -- Walls don't move

main = do
    rnd <- newStdGen
    let ball = Basic (Ball 200 100 5 5)
    let ghost = Basic (Ghost 100 100 rnd)
    let wall = Static (Wall 150 150)
    operateArena (Arena [ball, ghost, wall])
    -- try to add a Wall to the actors