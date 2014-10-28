module Data.Bencode.Format where

data BencodedData = Str String
                  | Int Integer
                  | List [BencodedData]
                  | Dict [(String, BencodedData)] deriving (Show, Eq)
