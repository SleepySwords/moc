module Utils where

scanUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
scanUntil p s v
  | p v = [v]
  | otherwise = v : scanUntil p s (s v)
