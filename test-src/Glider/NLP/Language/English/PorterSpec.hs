{-# LANGUAGE OverloadedStrings #-}
{- |
Module : Glider.NLP.Language.English.Porter2Test
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable
-}

module Glider.NLP.Language.English.PorterSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Glider.NLP.Language.English.Porter


spec :: Spec
spec = do

  describe "Porter" $ do
    it "consign" $ stem "consign" `shouldBe` "consign"
    it "class's" $ stem "class's" `shouldBe` "class'"
    it "classes" $ stem "classes" `shouldBe` "class"
    it "cried" $ stem "cried" `shouldBe` "cri"
    it "ties" $ stem "ties" `shouldBe` "ti"
    it "gas" $ stem "gas" `shouldBe` "ga"
    it "gaps" $ stem "gaps" `shouldBe` "gap"
    it "bleed" $ stem "bleed" `shouldBe` "bleed"
    it "guaranteed" $ stem "guaranteed" `shouldBe` "guarante"
