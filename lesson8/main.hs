myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake n (x : xs) = x : rest
  where
    rest = myTake (n - 1) xs

myCycle (first : rest) = first : myCycle (rest ++ [first])
