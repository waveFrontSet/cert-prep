{-# OPTIONS_GHC -Wno-orphans #-}

module Generators (
  arbitraryQuestion,
  mkQuestion,
  answerFor,
  arbitraryQAPair,
  correctlyAnsweredQAPair,
  questionsWithCategories,
  largeQuestionsWithCategories,
) where

import Test.QuickCheck

import CertPrep.Types

categoryPool :: [Text]
categoryPool =
  [ "AWS Storage",
    "AWS Compute",
    "AWS Networking",
    "AWS Security",
    "AWS Database"
  ]

mkQuestion :: Text -> [Text] -> [Int] -> Maybe Text -> Question
mkQuestion text choices correct cat =
  Question {
    text = text,
    answerChoices = choices,
    correctAnswer = fromList correct,
    category = cat
  }

arbitraryQuestion :: Gen Question
arbitraryQuestion = do
  text <- elements ["Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8"]
  numChoices <- chooseInt (2, 6)
  let choices = ["Choice " <> show i | i <- [1 .. numChoices]]
  numCorrect <- chooseInt (1, numChoices)
  correctIndices <-
    take numCorrect <$> shuffle [0 .. numChoices - 1]
  useCat <- arbitrary
  cat <-
    if useCat then
      Just <$> elements categoryPool
    else
      pure Nothing
  pure
    Question {
      text = text,
      answerChoices = choices,
      correctAnswer = fromList correctIndices,
      category = cat
    }

{- | Any selection a user could actually make for this question: a
(possibly empty) subset of the offered choice indices.
-}
answerFor :: Question -> Gen Answer
answerFor q = fromList <$> sublistOf [0 .. length (answerChoices q) - 1]

{- | Skewed towards the correct answer, which a plain 'answerFor' would
only hit once in 2^numChoices draws.
-}
arbitraryQAPair :: Gen (Question, Answer)
arbitraryQAPair = do
  q <- arbitraryQuestion
  a <- frequency [(1, pure (correctAnswer q)), (3, answerFor q)]
  pure (q, a)

correctlyAnsweredQAPair :: Gen (Question, Answer)
correctlyAnsweredQAPair = do
  q <- arbitraryQuestion
  pure (q, correctAnswer q)

questionsWithCategories :: [Text] -> Gen [Question]
questionsWithCategories cats = do
  perCat <- chooseInt (1, 5)
  concat <$> mapM (genForCat perCat) cats
 where
  genForCat count cat = do
    n <- chooseInt (1, count)
    vectorOf n $ do
      q <- arbitraryQuestion
      pure q {category = Just cat}

largeQuestionsWithCategories :: [Text] -> Gen [Question]
largeQuestionsWithCategories cats = do
  concat <$> mapM genForCat cats
 where
  genForCat cat = do
    n <- chooseInt (5, 10)
    vectorOf n $ do
      q <- arbitraryQuestion
      pure q {category = Just cat}

instance Arbitrary Question where
  arbitrary = arbitraryQuestion

titlePool :: [Text]
titlePool =
  [ "AWS Solutions Architect",
    "Azure Fundamentals",
    "GCP Associate",
    "Kubernetes Admin"
  ]

instance Arbitrary Config where
  arbitrary = do
    title <- elements titlePool
    qs <- listOf arbitraryQuestion
    n <- chooseInt (0, 100)
    useWeights <- arbitrary
    weights <-
      if useWeights then
        Just . fromList <$> listOf ((,) <$> elements categoryPool <*> chooseInt (1, 10))
      else
        pure Nothing
    pure
      Config {
        title = title,
        questions = qs,
        sampleAmount = n,
        categoryWeights = weights
      }
