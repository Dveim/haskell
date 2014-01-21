{-Вывести все предложения заданного текста (хранящегося в файле) в порядке возрастания количества слов в каждом из них-}

import Data.List
import Data.List.Split

myCompare :: String -> String -> Ordering
myCompare s1 s2
    | length s1 > length s2 = GT
    | length s1 < length s2 = LT
    | True = EQ

main :: IO ()
main = do
    s <- readFile =<< getLine
    print $ sortBy myCompare $ nub $ splitOneOf " !?.\n" s
