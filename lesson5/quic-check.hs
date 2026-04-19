ifEven f x =
  if even x
    then f x
    else x

genIfXEven x = (\f -> ifEven f x)

subtract2 = flip (-) 2

binaryPartialApplication :: (a -> b -> c) -> a -> (b -> c)
binaryPartialApplication f x = \y -> f x y
