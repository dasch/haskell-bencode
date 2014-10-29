import Test.Framework (defaultMain)

import Data.Bencode.Test

main :: IO ()
main = defaultMain [decodeSuite, encodingSuite]
