{-# Language NoMonomorphismRestriction #-}
import Data.Array

emptyBoard n = array ((1,1),(n,n)) [((i,j), 0) | i <- [1..n], j <- [1..n]]

showBoard b = 
    putStrLn $ unlines $ map (unwords . map (show . (b !))) indices 
        where 
            indices = [[(x, y) | x <- [startX..endX]] | y <- [startY..endY]] 
            ((startX, startY), (endX, endY)) = bounds b

decTo n x = reverse $ decTo' x
    where
        decTo' 0 = []
        decTo' y = let (a, b) = quotRem y n in [b] ++ decTo' a

decToBin = decTo 2

addZeros size l = 
    if length l < size
        then addZeros size ([0] ++ l)
        else l

addBishop b (x, y) = b // [((x, y), 1)]

combinations n = [addZeros size $ decToBin i | i <- [0..2^n - 1]]
    where 
        size = length $ decToBin $ 2^n - 1

resulting b [] = b
resulting b (x:xs) = 
    case x of
        0 | l == 0 -> resulting (addBishop b (1, 1)) xs
          | l == h - 1 -> resulting (addBishop b (1, h)) xs
          | otherwise -> resulting (addBishop (addBishop b (h, h - l)) (1, l + 1)) xs
        1 | l == 0 -> resulting (addBishop b (h, h)) xs
          | l == h - 1 -> resulting (addBishop b (h, 1)) xs
          | otherwise -> resulting (addBishop (addBishop b (h - l, h)) (l + 1, 1)) xs
        where
            ((_, _), (h, _)) = bounds b --board is a square, so h is it's size
            l = length xs

main = do
    n <- getLine
    mapM showBoard $ map (resulting $ emptyBoard $ read n) (combinations $ read n)