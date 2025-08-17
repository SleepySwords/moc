module Utils where

scanUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
scanUntil p s v
  | p v = [v]
  | otherwise = v : scanUntil p s (s v)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)
