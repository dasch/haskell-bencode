module Data.Bencode.Encoder where

import Data.Bencode.Format

bencode :: BencodedData -> String
bencode (Str str) = (show len) ++ ":" ++ str where len = length str
bencode (Int i) = "i" ++ (show i) ++ "e"
bencode (List elems) = "l" ++ content ++ "e" where content = concat (map bencode elems)
bencode (Dict pairs) = "d" ++ content ++ "e" where content = concat (map encodePair pairs)
                                                   encodePair (key, value) = bencode (Str key) ++ bencode value
