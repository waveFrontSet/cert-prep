module Sampling (
    SamplingStrategy (..),
    Weight,
    WeightMap,
    sampleQuestions,
) where

import Data.List (foldl', sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..), comparing)
import System.Random (RandomGen, SplitGen, splitGen, uniformR)
import Types (Category, Question (..))

type Weight = Int
type WeightMap = Map Category Weight
data SamplingStrategy
    = Uniform
    | Stratified WeightMap
    deriving (Show, Eq)

sampleQuestions ::
    (SplitGen g) => g -> Int -> SamplingStrategy -> [Question] -> [Question]
sampleQuestions _ n _ _ | n <= 0 = []
sampleQuestions gen n Uniform qs = sampleUniform gen n qs
sampleQuestions gen n (Stratified weights) qs = sampleStratified gen n weights qs

sampleUniform :: (SplitGen g) => g -> Int -> [Question] -> [Question]
sampleUniform _ _ [] = []
sampleUniform gen n qs
    | n >= length qs = qs
    | otherwise = take n $ shuffle gen qs

sampleStratified :: (SplitGen g) => g -> Int -> WeightMap -> [Question] -> [Question]
sampleStratified gen n weights qs =
    let grouped :: Map Category [Question]
        grouped =
            Map.fromListWith
                (++)
                [(cat, [q]) | q <- qs, Just cat <- [category q]]

        avails :: Map Category Int
        avails = Map.map length grouped

        totalAvailable = sum avails
        n' = min n totalAvailable

        allocations = allocateWithRemainder n' weights avails
     in concatSamples gen allocations grouped

{- | Allocate n slots across categories using largest-remainder method,
capping at available questions per category.
-}
allocateWithRemainder :: Int -> WeightMap -> Map Category Int -> Map Category Int
allocateWithRemainder n weights avails =
    let activeWeights = Map.intersectionWith const weights avails
        totalWeight = sum activeWeights
     in if totalWeight == 0
            then Map.empty
            else
                let idealPerCat :: Map Category Double
                    idealPerCat =
                        Map.map
                            ( \w ->
                                fromIntegral n
                                    * fromIntegral w
                                    / fromIntegral totalWeight
                            )
                            activeWeights

                    floorAlloc :: Map Category Int
                    floorAlloc = Map.map floor idealPerCat

                    remainders :: Map Category Double
                    remainders =
                        Map.intersectionWith
                            (\ideal fl -> ideal - fromIntegral fl)
                            idealPerCat
                            floorAlloc

                    allocated = sum floorAlloc
                    remaining = n - allocated

                    sortedByRemainder =
                        map fst $
                            sortBy (comparing (Down . snd)) $
                                Map.toList remainders

                    extraCats = take remaining sortedByRemainder

                    withExtra =
                        foldl' (flip (Map.adjust (+ 1))) floorAlloc extraCats
                 in capAndRedistribute withExtra avails

{- | Cap allocations at available question counts, redistributing surplus
to uncapped categories.
-}
capAndRedistribute :: Map Category Int -> Map Category Int -> Map Category Int
capAndRedistribute allocs avails =
    let (overAlloc, okAlloc) =
            Map.partitionWithKey
                (\k v -> v > Map.findWithDefault 0 k avails)
                allocs
     in if Map.null overAlloc
            then allocs
            else
                let capped =
                        Map.intersectionWith (\_ avail -> avail) overAlloc avails
                    surplus =
                        sum $ Map.intersectionWith (-) overAlloc capped
                    merged = Map.union capped okAlloc
                 in if surplus == 0
                        then merged
                        else
                            capAndRedistribute
                                (distributeEvenly surplus merged avails)
                                avails

-- | Distribute extra slots among categories that still have room.
distributeEvenly :: Int -> Map Category Int -> Map Category Int -> Map Category Int
distributeEvenly 0 m _ = m
distributeEvenly extra m avails =
    let eligible =
            Map.filterWithKey
                (\k v -> v < Map.findWithDefault 0 k avails)
                m
        eligibleCount = Map.size eligible
     in if eligibleCount == 0
            then m
            else
                let perCat = extra `div` eligibleCount
                    leftover = extra `mod` eligibleCount
                    keys = Map.keys eligible
                    bumped =
                        foldl'
                            (flip (Map.adjust (+ perCat)))
                            m
                            keys
                    withLeftover =
                        foldl'
                            (flip (Map.adjust (+ 1)))
                            bumped
                            (take leftover keys)
                 in withLeftover

concatSamples ::
    (SplitGen g) =>
    g ->
    Map Category Int ->
    Map Category [Question] ->
    [Question]
concatSamples gen0 allocs grouped = shuffle genShuffle $ go genSample cats
  where
    (genSample, genShuffle) = splitGen gen0
    cats = Map.toAscList allocs
    go _ [] = []
    go g ((cat, count) : rest) = sampled ++ go g2 rest
      where
        catQs = Map.findWithDefault [] cat grouped
        (g1, g2) = splitGen g
        sampled = sampleUniform g1 count catQs

shuffle :: (RandomGen g) => g -> [a] -> [a]
shuffle g xs =
    map snd $ sortBy (comparing fst) tagged
  where
    tagged = snd $ foldl' step (g, []) xs
    step (g', acc) x =
        let (r, g'') = uniformR (0 :: Int, maxBound) g'
         in (g'', (r, x) : acc)
