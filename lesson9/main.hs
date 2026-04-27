add3ToAll [] = []
add3ToAll (x : xs) = (3 + x) : add3ToAll xs

mul3ByAll [] = []
mul3ByAll (x : xs) = (3 * x) : mul3ByAll xs

funcToAll :: (a -> a) -> [a] -> [a]
funcToAll _ [] = []
funcToAll f (x : xs) = f x : funcToAll f xs
