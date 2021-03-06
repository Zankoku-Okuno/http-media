------------------------------------------------------------------------------
module Network.HTTP.Media.Match.Tests (tests) where

------------------------------------------------------------------------------
import Control.Monad                     (join, liftM, liftM2)
import Distribution.TestSuite.QuickCheck

------------------------------------------------------------------------------
import Network.HTTP.Media.Gen
import Network.HTTP.Media.Match


------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testMatches
    , testMoreSpecificThan
    , testMostSpecific
    ]


------------------------------------------------------------------------------
testMatches :: Test
testMatches = testGroup "matches"
    [ testProperty "Does match" $ do
        string <- genByteString
        return $ matches string string
    , testProperty "Doesn't match" $ do
        string  <- genByteString
        string' <- genDiffByteString string
        return . not $ matches string string'
    ]


------------------------------------------------------------------------------
-- | Note that this test never actually generates any strings, as they are not
-- required for the 'moreSpecificThan' test.
testMoreSpecificThan :: Test
testMoreSpecificThan = testProperty "moreSpecificThan" $
    join (liftM2 ((not .) . moreSpecificThan)) genByteString


------------------------------------------------------------------------------
testMostSpecific :: Test
testMostSpecific = testProperty "mostSpecific" $ do
    string <- genByteString
    liftM ((== string) . mostSpecific string) genByteString

