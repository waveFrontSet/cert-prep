module Sampling (sampleQuestions) where

import Data.List (sortBy)
import Data.Ord (comparing)
import System.Random (RandomGen, uniformR)
import Types (Question)

sampleQuestions :: (RandomGen g) => g -> Int -> [Question] -> [Question]
sampleQuestions _ 0 _ = []
sampleQuestions gen n qs
    | n >= length qs = qs
    | otherwise = map snd $ sortBy (comparing fst) $ take n $ shuffle gen (zip [0 ..] qs)
  where
    shuffle :: (RandomGen g) => g -> [(Int, a)] -> [(Int, a)]
    shuffle g xs =
        let (shuffled, _) = foldl shuffleStep ([], g) xs
         in shuffled

    shuffleStep :: (RandomGen g) => ([(Int, a)], g) -> (Int, a) -> ([(Int, a)], g)
    shuffleStep (acc, g) x =
        let (r, g') = uniformR (0 :: Int, 1000000) g
         in ((r, snd x) : acc, g')
