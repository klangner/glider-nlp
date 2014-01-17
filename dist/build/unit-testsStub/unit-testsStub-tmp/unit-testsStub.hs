module Main ( main ) where
import Distribution.Simple.Test ( stubMain )
import AllTests ( tests )
main :: IO ()
main = stubMain tests
