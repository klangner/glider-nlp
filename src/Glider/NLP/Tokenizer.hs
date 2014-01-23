{- |
Module : Glider.NLP.Tokenizer
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

This module contains functions which parses text into tokens. 
tokens are not normalized. If you need all tokens from the document then 
check function "tokenize". If you need only words (na dots, numbers etc.) 
then check function "getWords".

-}
module Glider.NLP.Tokenizer 
    ( Token(..)
    , foldCase
    , getWords
    , tokenize
    , wordParser
    , numberParser
    , punctuationParser
    , symbolParser
    , spaceParser
    , allParser
    ) where

import Prelude hiding (null, takeWhile, dropWhile, head, tail)    
import Data.Text
import Data.Char
import qualified Data.List as List

-- | Token type
data Token = Word Text
           | Number Text
           | Punctuation Char
           | Symbol Char
           | Whitespace
           | Unknown Char
           deriving (Eq, Show)


-- | Split text into tokens
--
-- > tokenize "one two." == [Word "one", Whitespace, Word "two", "Separator "."] 
tokenize :: Text -> [Token]
tokenize xs = case allParser xs of
    [(v, out)] -> v : tokenize out
    _ -> []
                
-- | Exctract all words from tokens
--
-- > getWords "one two." == ["one", "two"] 
getWords :: [Token] -> [Text]
getWords [] = []
getWords (x:xs) = case x of
                        Word a -> a: getWords xs
                        _ -> getWords xs
                        
-- | Convert all words to the same case
foldCase :: [Text] -> [Text]
foldCase = List.map toCaseFold                                          
    

-- | Parser type
type Parser = Text -> [(Token, Text)]

-- | Parse word
wordParser :: Parser
wordParser xs | null xs = []
              | isLetter (head xs) = [(Word (takeWhile isAlphaNum xs), dropWhile isAlphaNum xs)]
              | otherwise = []

-- | Parse number
numberParser :: Parser
numberParser xs | null xs = []
                | isDigit (head xs) = [(Number (takeWhile isDigit xs), dropWhile isDigit xs)]
                | otherwise = []

-- | Parse punctuation
punctuationParser :: Parser
punctuationParser xs | null xs = []
                     | isPunctuation (head xs) = [(Punctuation (head xs), tail xs)]
                     | otherwise = []

-- | Parse symbol
symbolParser :: Parser
symbolParser xs | null xs = []
                | isSymbol (head xs) = [(Symbol (head xs), tail xs)]
                | otherwise = []
                            
-- | Parse whitespaces
spaceParser :: Parser
spaceParser xs | null xs = []
               | isSpace (head xs) = [(Whitespace, dropWhile isSpace xs)]
               | otherwise = []
                            
-- | Parse single char
charParser :: Parser
charParser xs | null xs = []
              | otherwise = [(Unknown (head xs), tail xs)]

-- | Apply all parsers to the input. 
-- Return result from the first which will parse correctly given text.
allParser :: Parser
allParser xs = case wordParser xs of
                [(v, out)] -> [(v, out)]
                _ -> case numberParser xs of               
                        [(v, out)] -> [(v, out)]
                        _ -> case punctuationParser xs of               
                            [(v, out)] -> [(v, out)]
                            _ -> case symbolParser xs of               
                                [(v, out)] -> [(v, out)]
                                _ -> case spaceParser xs of               
                                    [(v, out)] -> [(v, out)]
                                    _ -> charParser xs
                