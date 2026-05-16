y :: Integer
y = 2

halve :: Integer -> Integer
halve n = div n 2

printDouble :: Int -> String
printDouble n = show (n * 2)

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n =
  if even n
    then f n
    else n

simple :: a -> a
simple x = x

makeTriple :: a -> b -> c -> (a, b, c)
makeTriple x y z = (x, y, z)
