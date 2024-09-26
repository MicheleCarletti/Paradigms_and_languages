doubleMe :: Num(a) => a -> a
doubleMe x = x + x

doubleUs :: Num(a) => a -> a -> a 
doubleUs x y = doubleMe x + doubleMe y

-- Use list comprehension to generate even numbers betwen 0 and n
evenNum :: Integral(a) => a -> [a]
evenNum n = [x | x <- [0 .. n], even x]

-- Compute factorial of a number
factorial :: Integral(a) => a -> a
factorial x = product[1..x]

-- Compute factorial with pattern matching
fact :: Integral(a) => a -> a
fact 0 = 1
fact n = n * fact(n - 1)

-- Matching lists
tell :: Show(a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ ", " ++ show y
tell (x:y:_) = "The list has more than two elements. It starts with: " ++ show x ++ ", " ++ show y

-- Compute length of a list
len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

-- Sum list's elements
sumEl :: Num(a) => [a] -> a
sumEl [] = 0
sumEl (x:xs) = x + sumEl xs

-- Compute product of elements
prod :: (Num a) => [a] -> a
prod [] = 1
prod (x:xs) = x * prod xs

capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Compute body mass index
bmiTell :: Float -> Float -> String
bmiTell weight height 
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Overweight"
    | otherwise = "Too overweight!"
    where bmi = weight / (height ** 2)

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | even n =  n:collatz (n `div` 2)
    | odd n  =  n:collatz (n*3 + 1)

collatzLen :: Integral(a) => a -> Int
collatzLen n = length (collatz n)

-- Find the maximum value
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list!"
maximum' [x] = x 
maximum' (x:xs) 
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

-- Find the minimum value
minimum' :: (Ord a) => [a] -> a
minimum' [] = error "Empty list!"
minimum' [x] = x
minimum' (x:xs)
    | x < minTail = x
    | otherwise = minTail
    where minTail = minimum' xs

-- Compute circle area given radius
circArea :: Float -> Float
circArea x = pi * (x ** 2)

-- Take first n values from a list
take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- Reverse elements of a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Create pairs from two lists
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- Check whether an element is part of a list
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) 
    | a == x = True
    | otherwise = (elem') a xs 

-- given a number n and an element, replicates it n times
replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x

-- Implement quicksort algorithm
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted 


-- Higher order functions
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

-- Same as
cwh :: (Num a, Ord a) => a -> Ordering
cwh = compare 100

-- Partially applied function
multiThree :: (Num a) => a -> a -> a -> a
multiThree x y z = x*y*z

-- Divide by ten
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- Function that takes another function as input
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)    

-- Implementation of zipWith function: it takes a function and two lists as parameters and then joins the two lists by applying the function
-- between corresponding elements
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

-- Implementation of filp function: it takes a function and returns a function similar to the original one, but the first two arguments are swapped
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y 

-- Define a map
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs 

-- Define a filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

-- A more readable version of Quincksort
qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (x:xs) =
    let smallerSorted = qs (filter' (<=x) xs)
        biggerSorted = qs (filter' (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

-- Find the largest number under 100,000 that's divisible by 32829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter' p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- Find how many chains computed from 1 to 100 have a length greater than 15 (use collatz function to compute the chain)
numLongChains :: Int
numLongChains = length (filter' isLong (map' collatz [1..100]))
    where isLong xs = length xs > 15

-- Rewrite the previous function but with lambda
nlc :: Int
nlc = length (filter' (\xs -> length xs > 15) (map' collatz [1..100]))

-- A function that returns strings longer than 3
longWords :: String -> [String]
longWords ws = filter' (\xs -> length xs > 3) (words ws)

-- Return the first word of a sentence
firstWord :: String -> String
firstWord ws = takeWhile (/=' ') ws

-- Reimplement sum with fold left
sumFl :: (Num a) => [a] -> a
sumFl xs = foldl (\acc x -> acc+x) 0 xs

-- Reimplement elem with fold left
elemFl :: (Eq a) => a -> [a] -> Bool
elemFl y ys = foldl (\acc x -> if x == y then True else acc) False ys 

-- Reimplement map with fold right
mapFr :: (a->b) -> [a] -> [b]
mapFr f xs = foldr (\x acc -> f x : acc) [] xs

-- Reimplement maximum using fold right 1
maxFr :: (Ord a) => [a] -> a
maxFr = foldr1 (\x acc -> if x > acc then x else acc)

-- Reimplement reverse using fold left
revFl :: [a] -> [a]
revFl = foldl (\acc x -> x : acc) []

-- Reimplement filter using fold right
filterFr :: (a -> Bool) -> [a] -> [a]
filterFr p = foldr (\x acc -> if p x then x : acc else acc) []

-- Function composition
-- A function that takes a list of values and makes all numbers negative
inv :: (Num a) => [a] -> [a]
inv xs = map (\x -> negate (abs x)) xs
-- Same function but with composition operator . insetad of lambda
inv2 :: (Num a) => [a] -> [a]
inv2 xs = map (negate . abs) xs
