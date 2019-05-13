import Data.Char

 -- This function returns the number of moves required to match two given strings as per the given operation. 
dubchanges::[Char]->[Char]->Int
dubchanges [] [] = 0
dubchanges (c1:s1) (c2:s2) = dubchanges s1 s2 + abs ((ord c1) - (ord c2))

 -- This function gets the number of required to make the input string a palindrome. 
getchanges::[Char]->Int
getchanges s = div (dubchanges s (reverse s)) 2