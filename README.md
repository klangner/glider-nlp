# Condor NLP tools

Condor is Haskell Natural Language Processing (NLP) library.

## Installation
The library can be installed from [Hackage](http://hackage.haskell.org/package/condor-nlp) with the command:

```sh
cabal install condor-nlp
```

## Usage

### Count number of words in the document

```haskell
import Condor.NLP.Statistics

doc <- readDocument "samples/haskell.txt"
countWords (docText doc)
```

### Get word frequency in the document

```haskell
import Data.Text
import Condor.NLP.Statistics

countWordsExample :: IO Int
countWordsExample = do
    doc <- readDocument "haskell.txt"
    return $ countWords (docText doc)

wordFreqExample :: IO [(Text, Int)]
wordFreqExample = do
    doc <- readDocument "haskell.txt"
    return $ wordFreq (docText doc)
```




This is alpha version of the library. It means that the API can change in the next releases.
