repeat' :: a -> [a]
repeat' x = cycle [x]

subseq :: Int -> Int -> [a] -> [a]
subseq start end xs = drop start $ take end xs

inFirstHalf :: (Eq a) => a -> [a] -> Bool
inFirstHalf x xs = x `elem` (take (length xs `div` 2) xs)
