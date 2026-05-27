simple :: p -> p
simple x = x

aList :: [String]
aList = ["cat", "dog", "mouse"]

addThenDouble :: (Num a) => a -> a -> a
addThenDouble x y = (x + y) * 2

class Describable a where
  describe :: a -> String
