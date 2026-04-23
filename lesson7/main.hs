myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)

sayAmount 1 = "one"
sayAmount 2 = "two"
sayAmount n = "a bunch"

myHead (x : xs) = x
myHead [] = error "No head for empty list"

myTail [] = []
myTail (_ : xs) = xs
