simple :: p -> p
simple x = x

aList :: [String]
aList = ["cat", "dog", "mouse"]

addThenDouble :: (Num a) => a -> a -> a
addThenDouble x y = (x + y) * 2

class Describable a where
  describe :: a -> String

data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n =
  if n == maxBound
    then minBound
    else succ n
