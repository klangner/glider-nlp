[![Hackage](https://img.shields.io/hackage/v/radium.svg)](https://hackage.haskell.org/package/glider-nlp)

# Glider NLP

glider-nlp is Haskell library for processing natural language.

## Installation
The library can be installed from [Hackage](http://hackage.haskell.org/package/glider-nlp) with the command:

```sh
cabal install glider-nlp
```

or build from the source
```bash
stack setup
stack build
stack test
```

## Usage

### Count number of words in the document

```haskell
import Glider.NLP.Statistics

doc <- readDocument "samples/haskell.txt"
countWords (docText doc)
```

### Get word frequency in the document

```haskell
import Data.Text
import Glider.NLP.Statistics

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
