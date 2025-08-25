module Utils where
import Data.Maybe (fromJust, isNothing)

-- This is suspiciously like a monad, could probably chain it
scanUntil :: (a -> Maybe a) -> a -> [a]
scanUntil s v
  | isNothing (s v) = [v]
  | otherwise = v : scanUntil s (fromJust $ s v)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapThd :: (c -> d) -> (a, b, c) -> (a, b, d)
mapThd f (a, b, c) = (a, b, f c)
