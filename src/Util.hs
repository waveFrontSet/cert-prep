module Util (formatTime) where

formatTime :: Int -> String
formatTime totalSecs =
    pad mins ++ ":" ++ pad secs
  where
    mins = totalSecs `div` 60
    secs = totalSecs `mod` 60
    pad n = if n < 10 then "0" ++ show n else show n
