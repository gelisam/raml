module Data.List.MyExtra where


-- |
-- >>> notAll even [2,4,6]
-- False
-- >>> notAll even [2,4,6,1]
-- True
notAll :: (a -> Bool) -> [a] -> Bool
notAll p = not . all p

-- |
-- >>> none even [1,3,5]
-- True
-- >>> none even [1,3,5,4]
-- False
none :: (a -> Bool) -> [a] -> Bool
none p = not . any p
