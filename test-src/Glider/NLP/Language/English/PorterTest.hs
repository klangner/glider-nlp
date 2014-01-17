{- |
Module : Glider.NLP.Language.English.Porter2Test
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable
-}

module Glider.NLP.Language.English.PorterTest (testCases) where

import qualified Data.Text as T
import Glider.NLP.Language.English.Porter
import Test.HUnit


testCases :: [(String, Test)]
testCases = [("Porter", t) | t <- tests]

tests :: [Test]
tests = [ TestCase $ prop_stem "consign" "consign"
        , TestCase $ prop_stem "class's" "class'"
        , TestCase $ prop_stem "classes" "class"
        , TestCase $ prop_stem "cried" "cri"
        , TestCase $ prop_stem "ties" "ti"
        , TestCase $ prop_stem "gas" "ga"
        , TestCase $ prop_stem "gaps" "gap"
        , TestCase $ prop_stem "bleed" "bleed"
        , TestCase $ prop_stem "guaranteed" "guarante"
        ]
         
prop_stem :: String -> String -> Assertion         
prop_stem a b = assertEqual a (T.pack b) (stem (T.pack a))          
