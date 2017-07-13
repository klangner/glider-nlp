{-# LANGUAGE OverloadedStrings #-}
{- |
Module : Glider.NLP.Statistics
Copyright : Copyright (C) 2013-2016 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

This module counts different statistics on the text

-}
module Glider.NLP.Statistics 
    ( countWords
    , wordFreq
    ) where

import Prelude hiding(length)
import Data.Text
import Glider.NLP.Tokenizer
import qualified Data.List as L


-- | Count number of words in the text.
--
-- > countWords "one two three" == 3
countWords :: Text -> Int    
countWords = L.length . getWords . tokenize

-- | Count word frequency
--
-- > wordFreq "one two, three one" == [("one", 2), ("two", 1), ("three", 1)]
wordFreq :: Text -> [(Text, Int)]
wordFreq txt = [(L.head xs, L.length xs) | xs <- L.group tokens]
    where tokens = L.sort $ (foldCase . getWords . tokenize) txt
