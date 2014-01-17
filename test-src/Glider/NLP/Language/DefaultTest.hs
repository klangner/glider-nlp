{- |
Module : Glider.NLP.Language.DefaultTest
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable
-}

module Glider.NLP.Language.DefaultTest (testCases) where

import Data.Text 
import Glider.NLP.Language.Default
import Test.HUnit


testCases :: [(String, Test)]
testCases = [ ( "Less then 4 chars" 
              , TestCase $ prop_isStopWord "abc" True)
            , ( "4 chars not stop word" 
              , TestCase $ prop_isStopWord "abcd" False)
            ]

-- | Check empty index         
prop_isStopWord :: String -> Bool -> Assertion         
prop_isStopWord a e = assertEqual a e (isStopWord (pack a))          
              