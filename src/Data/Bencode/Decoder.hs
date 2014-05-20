import Data.Bencode

decode input = case bdecode input of
    Left error -> show error
    Right value -> show value

main = do
    input <- getContents
    putStrLn $ decode input
