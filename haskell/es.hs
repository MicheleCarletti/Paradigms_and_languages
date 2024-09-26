somma :: Int -> Int
somma 0 = 0
somma n = n + somma (n-1)

fattoriale :: Int -> Int
fattoriale 0 = 1
fattoriale n = n * fattoriale (n-1)

pari :: [Int] -> [Int]
pari [] = []
pari xs = filter even xs

listaNumeri :: Int -> [Int]
listaNumeri 0 = []
listaNumeri n = listaNumeri (n-1) ++ [n]

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
    | p x = x : myFilter p xs
    | otherwise = myFilter p xs

palindromo :: (Eq a) => [a] -> Bool
palindromo xs = if xs == reverse xs
                then True
                else False

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

rimpiazza :: (Eq a) => a -> a -> [a] -> [a]
rimpiazza _ _ [] = []
rimpiazza a b (x:xs)
    | a == x = b : rimpiazza a b xs
    | otherwise = x : rimpiazza a b xs

myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl _ acc [] = acc
myfoldl f acc (x:xs) = myfoldl f (f acc x) xs

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr _ acc [] = acc
myfoldr f acc (x:xs) = f x (myfoldr f acc xs)


{-
sommaPrimi n restituisce la somma dei numeri primi compresi tra 1 ed n
Si utilizza una funzione ausiliaria per verificare se un numero è primo,
a questo punto si filtra la lista [1..n] e si sommano i valori restanti
-}

isPrime :: Int -> Bool
isPrime k
    | k < 2 = False
    | otherwise = null [x | x <- [2..(floor . sqrt $ fromIntegral k)], k `mod` x == 0] -- Cerca i divisori fino alla radice quadrata di k.
                                                                                       -- Se non esistono (null) allora k è primo

sommaPrimi :: Int -> Int
sommaPrimi n = sum [x | x <- [2 .. n], isPrime x]

primi :: Int -> [Int]
primi n = [x | x <- [2 .. n], isPrime x]

-- Prende una lista di funzioni e le compone da destra a sinistra
composeFunc :: [(a -> a)] -> (a -> a)
composeFunc [] = id
composeFunc (x:xs) = x . composeFunc(xs)
