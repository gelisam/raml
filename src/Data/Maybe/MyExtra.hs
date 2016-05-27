module Data.Maybe.MyExtra where


maybeAp :: (a -> b -> b)
        -> Maybe a -> b -> b
maybeAp f = maybe id f
