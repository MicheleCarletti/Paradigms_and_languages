-- Read continously a line and check wheter the string is or is not palindrome
-- Using interact insead getContents

respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not palindrome" ) . lines
    where isPalindrome xs = xs == reverse xs

main = interact respondPalindromes 