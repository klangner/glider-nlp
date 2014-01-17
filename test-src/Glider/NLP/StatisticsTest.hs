{- |
Module : Glider.NLP.IndexTest
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable
-}

module Glider.NLP.StatisticsTest (testCases) where

import qualified Data.Text as T
import Glider.NLP.Statistics
import Test.HUnit


testCases :: [(String, Test)]
testCases = [ ( "Count words"
              , TestCase $ prop_countWords "one two three" 3)
            
            , ( "Single word frequency"
              , TestCase $ prop_wordFreq "one" [("one", 1)])
            
            , ( "2 words frequency"
              , TestCase $ prop_wordFreq "one two" [("one", 1), ("two", 1)])
            
            , ( "Multi words frequency"
              , TestCase $ prop_wordFreq "one two one" [("one", 2), ("two", 1)])
            ]
         
-- | Count number of words       
prop_countWords :: String -> Int ->  Assertion         
prop_countWords xs n = assertEqual xs n $ countWords (T.pack xs)

-- | Check word frequency          
prop_wordFreq :: String -> [(String, Int)] ->  Assertion         
prop_wordFreq xs es = assertEqual xs expected $ wordFreq (T.pack xs)
    where expected = [(T.pack a, b) | (a, b) <- es]
