import Data.Char (toLower)

add3ToAll [] = []
add3ToAll (x : xs) = (3 + x) : add3ToAll xs

mul3ByAll [] = []
mul3ByAll (x : xs) = (3 * x) : mul3ByAll xs

funcToAll :: (a -> a) -> [a] -> [a]
funcToAll _ [] = []
funcToAll f (x : xs) = f x : funcToAll f xs

addAnA :: [String] -> [String]
addAnA [] = []
addAnA (x : xs) = ("a " ++ x) : addAnA xs

squareAll :: (Num a) => [a] -> [a]
squareAll [] = []
squareAll (x : xs) = x ^ 2 : squareAll xs

myMap f [] = []
myMap f (x : xs) = (f x) : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter test [] = []
myFilter test (x : xs) =
  if test x
    then x : myFilter test xs
    else myFilter test xs

remove :: (a -> Bool) -> [a] -> [a]
remove test [] = []
remove test (x : xs) =
  if test x
    then remove test xs
    else x : remove test xs

myProduct :: (Num a) => [a] -> a
myProduct xs = foldl (*) 1 xs

concatAll xs = foldl (++) "" xs

sumOfSquares xs = foldl (+) 0 (map (^ 2) xs)

rcons x y = y : x

myReverse xs = foldl rcons [] xs

myFoldl f init [] = init
myFoldl f init (x : xs) = myFoldl f newInit xs
  where
    newInit = f init x

myFoldr f init [] = init
myFoldr f init (x : xs) = f x rightResult
  where
    rightResult = myFoldr f init xs

myElem :: (Eq a) => a -> [a] -> Bool
myElem v myList = length filterdList > 0
  where
    filterdList = filter (== v) myList

isPalindrome :: String -> Bool
isPalindrome text = processedText == reverse processedText
  where
    noSpaces = filter (/= ' ') text
    processedText = map toLower noSpaces
