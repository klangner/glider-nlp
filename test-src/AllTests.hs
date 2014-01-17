module AllTests (tests) where

import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as H
import Glider.NLP.TokenizerTest (testCases)
import Glider.NLP.StatisticsTest (testCases)
import Glider.NLP.Language.English.PorterTest (testCases)
import Glider.NLP.Language.DefaultTest (testCases)


tests :: IO [C.Test]
tests = return $ map (uncurry H.test) $  Glider.NLP.TokenizerTest.testCases
                                      ++ Glider.NLP.Language.English.PorterTest.testCases
                                      ++ Glider.NLP.Language.DefaultTest.testCases
                                      ++ Glider.NLP.StatisticsTest.testCases
                                      
                                      