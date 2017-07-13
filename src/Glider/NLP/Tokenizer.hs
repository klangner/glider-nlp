{-# LANGUAGE OverloadedStrings #-}
{- |
Module : Glider.NLP.Tokenizer
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

This module contains functions which parses text into tokens. 
Tokens are not normalized. If you need all tokens from the document then
use function "tokenize". If you need only words (no dots, numbers etc.)
then check function "getWords".

-}
module Glider.NLP.Tokenizer 
    ( Token(..)
    , foldCase
    , getWords
    , tokenize
    ) where

import Prelude hiding (null, takeWhile, dropWhile, head, tail)
import Text.Parsec ((<|>), parse, digit, letter, many, many1)
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (space)
import Data.Text
import qualified Data.List as List

-- | Token type
data Token = Word Text
           | Number Text
           | Punctuation Char
           | Symbol Char
           | Whitespace
           | Unknown Char
           deriving (Eq, Show)


-- | Extract all words from tokens
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


-- Split text into tokens
--
-- > tokenize "one two." == [Word "one", Whitespace, Word "two", "Separator "."] 
tokenize :: Text -> [Token]
tokenize str = case parse tokenParser "" str of
    Left _ -> []
    Right val -> val


-- Consume all tokens.
tokenParser :: Parser [Token]
tokenParser = many (word <|> number <|> whitespace)

-- Word Parser
word :: Parser Token
word = do
  xs <- many1 letter
  return $ Word (pack xs)

-- Parse number. Number consists of many digits
number :: Parser Token
number =  do
  xs <- many1 digit
  return $ Number (pack xs)

-- Whitespace Parser
whitespace :: Parser Token
whitespace = do
  _ <- many1 space
  return Whitespace