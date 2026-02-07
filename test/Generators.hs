{-# OPTIONS_GHC -Wno-orphans #-}

module Generators (
    arbitraryQuestion,
    mkQuestion,
    questionsWithCategories,
    largeQuestionsWithCategories,
) where

import Data.IntSet qualified as IS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Test.QuickCheck
import Types (Config (..), Question (..))

categoryPool :: [Text]
categoryPool =
    [ "AWS Storage"
    , "AWS Compute"
    , "AWS Networking"
    , "AWS Security"
    , "AWS Database"
    ]

mkQuestion :: Text -> [Text] -> [Int] -> Maybe Text -> Question
mkQuestion text choices correct cat =
    Question
        { questionText = text
        , questionAnswerChoices = choices
        , questionCorrectAnswer = IS.fromList correct
        , questionCategory = cat
        }

arbitraryQuestion :: Gen Question
arbitraryQuestion = do
    text <- elements ["Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8"]
    numChoices <- chooseInt (2, 6)
    let choices = ["Choice " <> T.pack (show i) | i <- [1 .. numChoices]]
    numCorrect <- chooseInt (1, numChoices)
    correctIndices <-
        take numCorrect <$> shuffle [0 .. numChoices - 1]
    useCat <- arbitrary
    cat <-
        if useCat
            then Just <$> elements categoryPool
            else pure Nothing
    pure
        Question
            { questionText = text
            , questionAnswerChoices = choices
            , questionCorrectAnswer = IS.fromList correctIndices
            , questionCategory = cat
            }

questionsWithCategories :: [Text] -> Gen [Question]
questionsWithCategories cats = do
    perCat <- chooseInt (1, 5)
    concat <$> mapM (genForCat perCat) cats
  where
    genForCat count cat = do
        n <- chooseInt (1, count)
        vectorOf n $ do
            q <- arbitraryQuestion
            pure q{questionCategory = Just cat}

largeQuestionsWithCategories :: [Text] -> Gen [Question]
largeQuestionsWithCategories cats = do
    concat <$> mapM genForCat cats
  where
    genForCat cat = do
        n <- chooseInt (5, 10)
        vectorOf n $ do
            q <- arbitraryQuestion
            pure q{questionCategory = Just cat}

instance Arbitrary Question where
    arbitrary = arbitraryQuestion

instance Arbitrary Config where
    arbitrary = do
        qs <- listOf arbitraryQuestion
        n <- chooseInt (0, 100)
        useWeights <- arbitrary
        weights <-
            if useWeights
                then Just . Map.fromList <$> listOf ((,) <$> elements categoryPool <*> chooseInt (1, 10))
                else pure Nothing
        pure
            Config
                { configQuestions = qs
                , configSampleAmount = n
                , configCategoryWeights = weights
                }
