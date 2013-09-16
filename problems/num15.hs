{-Вывести все предложения заданного текста (хранящегося в файле) в порядке возрастания количества слов в каждом из них-}

import Data.List

splitToSentences :: String -> [String]
splitToSentences "" = []
splitToSentences s = let (l, s') = break (`elem` ['.', '!', '?']) s
    in l : 
        case s' of
            [] -> []
            (_:s'') -> splitToSentences s''

myCompare s1 s2
    | length s1 > length s2 = GT
    | length s1 < length s2 = LT
    | True = EQ
    
main = do
    s <- readFile =<< getLine
    print $ sortBy myCompare $ splitToSentences s