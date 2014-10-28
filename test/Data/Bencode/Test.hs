module Data.Bencode.Test where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Data.Bencode
import Data.Bencode.Format

bencodeSuite :: Test
bencodeSuite = testGroup "bencode" [
    testCase "positive integer" $ testInput "i42e" (Int 42),
    testCase "negative integer" $ testInput "i-42e" (Int (-42)),
    testCase "empty string" $ testInput "0:" (Str ""),
    testCase "non-empty string" $ testInput "3:foo" (Str "foo"),
    testCase "empty list" $ testInput "le" (List []),
    testCase "non-empty list" $ testInput "li1ei2ee" (List [Int 1, Int 2]),
    testCase "empty dict" $ testInput "de" (Dict []),
    testCase "non-empty dict" $ testInput "d3:fooi42ee" (Dict [("foo", Int 42)])
    ]

testInput :: String -> BencodedData -> Assertion
testInput input expected = actual @=? expected where Right actual = bdecode input
