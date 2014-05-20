module Data.Bencode.Parser (bdecode) where

import Data.Bencode.Format
import Text.ParserCombinators.Parsec

bdecode :: String -> Either ParseError BencodedData
bdecode input = parse parseString "(unknown)" input

parseString = do
    value <- parseValue
    optional newline
    eof
    return value

parseValue = choice [parseInt, parseList, parseDict, parseStr]

parseInt = do
    char 'i'
    value <- parseInteger
    char 'e'
    return value

parseInteger = do
    sign <- parseIntegerSign
    number <- parseIntegerNumber
    return $ Int (sign * number)

parseIntegerSign = option 1 $ do
    char '-'
    return (-1)

parseIntegerNumber = do
    digits <- many1 digit
    return $ read digits

parseStr = do
    length <- many1 digit
    char ':'
    content <- count (read length) anyToken
    return $ Str content

parseList = do
    char 'l'
    values <- many parseValue
    char 'e'
    return $ List values

parseDict = do
    char 'd'
    elements <- many parseDictElement
    char 'e'
    return $ Dict elements

parseDictElement = do
    Str key <- parseStr
    value <- parseValue
    return (key, value)
