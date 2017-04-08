module Data.Bencode.Parser (bdecode) where

import Data.Bencode.Format
import Text.ParserCombinators.Parsec

bdecode :: String -> Either ParseError BencodedData
bdecode input = parse datum "(unknown)" input

datum = do
    val <- value
    optional newline
    eof
    return val

value = choice [integer, list, dict, str]

integer = do
    char 'i'
    sign <- integerSign
    number <- integerNumeral
    char 'e'
    return $ Int (sign * number)

integerSign = option 1 $ do
    char '-'
    return (-1)

integerNumeral = do
    digits <- many1 digit
    return $ read digits

str = do
    length <- many1 digit
    char ':'
    content <- count (read length) anyToken
    return $ Str content

list = do
    char 'l'
    values <- many value
    char 'e'
    return $ List values

dict = do
    char 'd'
    elements <- many dictElement
    char 'e'
    return $ Dict elements

dictElement = do
    Str key <- str
    value <- value
    return (key, value)
