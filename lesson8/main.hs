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

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fastFib :: Int -> Int -> Int -> Int
fastFib n1 n2 0 = n1
fastFib n1 n2 counter = fastFib n2 (n1 + n2) (counter - 1)
