{- |
Module : Glider.NLP.Language.Default
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

This module contains default implementation for language specific algorithms.
If there is no specific algorithm for yor language you can use one from this module.

-}
module Glider.NLP.Language.Default (isStopWord) where
    
import Prelude hiding (length)    
import Data.Text
    

-- | Any less then 4 characters words is marked as stop word
isStopWord :: Text -> Bool
isStopWord w = length w < 4
