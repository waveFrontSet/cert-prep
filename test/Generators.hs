{-# OPTIONS_GHC -Wno-orphans #-}

module Generators (
    arbitraryQuestion,
    questionsWithCategories,
    largeQuestionsWithCategories,
) where

import Data.IntSet qualified as IS
import Test.QuickCheck
import Types (Question (..))

categoryPool :: [String]
categoryPool =
    [ "AWS Storage"
    , "AWS Compute"
    , "AWS Networking"
    , "AWS Security"
    , "AWS Database"
    ]

arbitraryQuestion :: Gen Question
arbitraryQuestion = do
    text <- elements ["Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8"]
    numChoices <- chooseInt (2, 6)
    let choices = ["Choice " ++ show i | i <- [1 .. numChoices]]
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

questionsWithCategories :: [String] -> Gen [Question]
questionsWithCategories cats = do
    perCat <- chooseInt (1, 5)
    concat <$> mapM (genForCat perCat) cats
  where
    genForCat count cat = do
        n <- chooseInt (1, count)
        vectorOf n $ do
            q <- arbitraryQuestion
            pure q{questionCategory = Just cat}

largeQuestionsWithCategories :: [String] -> Gen [Question]
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
