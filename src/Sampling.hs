module Sampling (
    SamplingStrategy (..),
    sampleQuestions,
) where

import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import System.Random (RandomGen, uniformR)
import Types (Question (..))

data SamplingStrategy
    = Uniform
    | Stratified (Map String Int)
    deriving (Show, Eq)

sampleQuestions ::
    (RandomGen g) => g -> Int -> SamplingStrategy -> [Question] -> [Question]
sampleQuestions _ n _ _ | n <= 0 = []
sampleQuestions gen n Uniform qs = sampleUniform gen n qs
sampleQuestions gen n (Stratified weights) qs = sampleStratified gen n weights qs

sampleUniform :: (RandomGen g) => g -> Int -> [Question] -> [Question]
sampleUniform _ _ [] = []
sampleUniform gen n qs
    | n >= length qs = qs
    | otherwise = take n $ shuffle gen qs

sampleStratified ::
    (RandomGen g) => g -> Int -> Map String Int -> [Question] -> [Question]
sampleStratified gen n weights qs =
    let grouped :: Map String [Question]
        grouped =
            Map.filterWithKey (\k _ -> Map.member k weights) $
                foldl
                    ( \acc q -> case questionCategory q of
                        Just cat -> Map.insertWith (++) cat [q] acc
                        Nothing -> acc
                    )
                    Map.empty
                    qs

        avails :: Map String Int
        avails = Map.map length grouped

        totalAvailable = sum avails
        n' = min n totalAvailable

        allocations = allocateWithRemainder n' weights avails
     in concatSamples gen allocations grouped

{- | Allocate n slots across categories using largest-remainder method,
capping at available questions per category.
-}
allocateWithRemainder :: Int -> Map String Int -> Map String Int -> Map String Int
allocateWithRemainder n weights avails =
    let activeWeights = Map.intersectionWith const weights avails
        totalWeight = sum activeWeights
     in if totalWeight == 0
            then Map.empty
            else
                let idealPerCat :: Map String Double
                    idealPerCat =
                        Map.map
                            ( \w ->
                                fromIntegral n
                                    * fromIntegral w
                                    / fromIntegral totalWeight
                            )
                            activeWeights

                    floorAlloc :: Map String Int
                    floorAlloc = Map.map floor idealPerCat

                    remainders :: Map String Double
                    remainders =
                        Map.intersectionWith
                            (\ideal fl -> ideal - fromIntegral fl)
                            idealPerCat
                            floorAlloc

                    allocated = sum floorAlloc
                    remaining = n - allocated

                    sortedByRemainder =
                        map fst $
                            sortBy (flip $ comparing snd) $
                                Map.toList remainders

                    extraCats = take remaining sortedByRemainder

                    withExtra =
                        foldl (flip (Map.adjust (+ 1))) floorAlloc extraCats
                 in capAndRedistribute withExtra avails

{- | Cap allocations at available question counts, redistributing surplus
to uncapped categories.
-}
capAndRedistribute :: Map String Int -> Map String Int -> Map String Int
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
distributeEvenly :: Int -> Map String Int -> Map String Int -> Map String Int
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
                        foldl
                            (flip (Map.adjust (+ perCat)))
                            m
                            keys
                    withLeftover =
                        foldl
                            (flip (Map.adjust (+ 1)))
                            bumped
                            (take leftover keys)
                 in withLeftover

concatSamples ::
    (RandomGen g) =>
    g ->
    Map String Int ->
    Map String [Question] ->
    [Question]
concatSamples gen0 allocs grouped =
    let cats = Map.toAscList allocs
     in go gen0 cats
  where
    go _ [] = []
    go g ((cat, count) : rest) =
        let catQs = Map.findWithDefault [] cat grouped
            (g1, g2) = splitGen g
            sampled = sampleUniform g1 count catQs
         in sampled ++ go g2 rest

splitGen :: (RandomGen g) => g -> (g, g)
splitGen g =
    let (_, g1) = uniformR (0 :: Int, maxBound) g
        (_, g2) = uniformR (0 :: Int, maxBound) g1
     in (g1, g2)

shuffle :: (RandomGen g) => g -> [a] -> [a]
shuffle g xs =
    map snd $ sortBy (comparing fst) tagged
  where
    tagged = snd $ foldl step (g, []) xs
    step (g', acc) x =
        let (r, g'') = uniformR (0 :: Int, maxBound) g'
         in (g'', (r, x) : acc)
