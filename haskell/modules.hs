import Data.List

-- Function counting the unique values in alist using nub function of Data.List
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub   -- same as \xs -> length (nub xs)
-- to load a module in GHCI do :m + Data.List

-- Implement function to search a list for a sublist (like isInfixOf function)
src :: (Eq a) => [a] -> [a] -> Bool
src needle haystack =
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- Use Algebric data type to define our own datatype
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float 
            | Rectangle Point Point
            deriving (Show)

-- A function that computes the surface of a Shape
surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle ( Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- Nudge a Shape of a certain offset
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
        Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- Define a new type with Record Syntax
data Person = Person {
    firstName :: String,
    lastName :: String,
    age :: Int
} deriving (Show, Eq)

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

data Days = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Type synonyms 
type Name = String
type PhoneNumber = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook n num book = (n,num) `elem` book

-- Given two people p1 and p2 check whether p1 is older than p2
isOlder :: Person -> Person -> Bool
isOlder p1 p2  
    | age p1 > age p2 = True
    | otherwise = False

-- Same as the previous one but with lambda function
olderThan :: Person -> Person -> Bool
olderThan = (\p1 p2 -> if age p1 > age p2 then True else False)