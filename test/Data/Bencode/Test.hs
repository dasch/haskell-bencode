module Data.Bencode.Test where

import Control.Monad

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test)
import Test.QuickCheck

import Data.Bencode
import Data.Bencode.Encoder
import Data.Bencode.Format

decodeSuite :: Test
decodeSuite = testGroup "decoding" [
    testCase "positive integer" $ testInput "i42e" (Int 42),
    testCase "negative integer" $ testInput "i-42e" (Int (-42)),
    testCase "empty string" $ testInput "0:" (Str ""),
    testCase "non-empty string" $ testInput "3:foo" (Str "foo"),
    testCase "empty list" $ testInput "le" (List []),
    testCase "non-empty list" $ testInput "li1ei2ee" (List [Int 1, Int 2]),
    testCase "empty dict" $ testInput "de" (Dict []),
    testCase "non-empty dict" $ testInput "d3:fooi42ee" (Dict [("foo", Int 42)]),
    testCase "dict with empty key" $ testInput "d0:i42ee" (Dict [("", Int 42)])
    ]

encodingSuite :: Test
encodingSuite = testGroup "encoding" [
    testProperty "identity" prop_identity
    ]

testInput :: String -> BencodedData -> Assertion
testInput input expected = actual @=? expected where Right actual = bdecode input

prop_identity :: BencodedData -> Bool
prop_identity input = output == input where Right output = bdecode (bencode input)

instance Arbitrary BencodedData where
    arbitrary = oneof [arbitraryInt, arbitraryStr, arbitraryList, arbitraryDict]
        where
            arbitraryInt = liftM Int arbitrary
            arbitraryStr = liftM Str arbitrary
            arbitraryList = do
                elems <- listOf arbitraryInt
                return $ List elems
            arbitraryDict = liftM Dict arbitrary
