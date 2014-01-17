{- |
Module : Glider.NLP.TokenizerTest
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable
-}

module Glider.NLP.TokenizerTest (testCases) where

import qualified Data.Text as T
import Glider.NLP.Tokenizer
import Test.HUnit


testCases :: [(String, Test)]
testCases = [ ( "Empty string"
              , TestCase $ prop_tokenize [] ""
              )
              
            , ( "Empty string"
              , TestCase $ prop_tokenize [ Word $ T.pack "one"
                                         , Whitespace
                                         , Word $ T.pack "two"
                                         , Whitespace
                                         , Word $ T.pack "three"
                                         ] 
                                         "one two three"
              )  
            ]
         
prop_tokenize :: [Token] -> String -> Assertion         
prop_tokenize e a = assertEqual a e (tokenize (T.pack a))          