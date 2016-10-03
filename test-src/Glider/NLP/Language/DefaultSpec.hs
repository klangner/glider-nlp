{-# LANGUAGE OverloadedStrings #-}
{- |
Module : Glider.NLP.Language.DefaultSpec
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable
-}

module Glider.NLP.Language.DefaultSpec (spec) where

import Test.Hspec
import Glider.NLP.Language.Default


spec :: Spec
spec = describe "Stop word" $ do
    it "less then 4 chars." $ isStopWord "ala" `shouldBe` True
    it "4 chars." $ isStopWord "abcd" `shouldBe` False
