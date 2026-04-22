paExample1 = (!!) "dog"

paExample2 = ("dog" !!)

paExample3 = (!! 2)

isPalindrome :: String -> Bool
isPalindrome word = word == reverse word

respond :: String -> String
respond phrase =
  if '!' `elem` phrase
    then "wow!"
    else "uh.. okey"

assignToGroups :: (Num a, Enum a) => a -> [b] -> [(a, b)]
assignToGroups n = zip groups
  where
    groups = cycle [1 .. n]
