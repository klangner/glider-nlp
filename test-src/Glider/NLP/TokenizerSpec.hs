{-# LANGUAGE OverloadedStrings #-}
{- |
Module : Glider.NLP.TokenizerSpec
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable
-}

module Glider.NLP.TokenizerSpec (spec) where

import Glider.NLP.Tokenizer
import Test.Hspec


spec :: Spec
spec = describe "Tokenize" $ do

    it "empty string" $ tokenize "" `shouldBe` []

    it "text strings" $ tokenize "one two three" `shouldBe` [ Word "one"
                                                             , Whitespace
                                                             , Word "two"
                                                             , Whitespace
                                                             , Word "three"
                                                             ]
