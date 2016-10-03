{-# LANGUAGE OverloadedStrings #-}
{- |
Module : Glider.NLP.IndexTest
Copyright : Copyright (C) 2013-2016 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable
-}

module Glider.NLP.StatisticsSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Glider.NLP.Statistics


spec :: Spec
spec = do

  describe "countWords" $ do
    it "text tokens" $ countWords "one two three" `shouldBe` 3

  describe "wordFreq" $ do
    it "single word" $ wordFreq "one" `shouldBe` [("one", 1)]
    it "2 words frequency" $ wordFreq "one two" `shouldBe` [("one", 1), ("two", 1)]
    it "Multi words frequency" $ wordFreq "one two one" `shouldBe` [("one", 2), ("two", 1)]
