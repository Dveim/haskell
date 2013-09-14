{-Найти такой путь коня на шахматной доске, чтобы в каждой клетке он побывал только один раз.-}

import Data.Array
import Data.List

emptyBoard n = array ((-1, -1),(n+2, n+2)) 
    [if 1 <= i && i <= n && 1 <= j && j <= n
        then ((i, j), 1) 
        else ((i, j), 0) | i <- [-1..n+2], j <- [-1..n+2]]

addKnight b (x, y) = b // [((x, y), 0)]
        
inBoard b (i, j) = (startX+2 <= i && i <= endX-2 && startY+2 <= j && j <= endY-2) --ugly =(
    where 
        ((startX, startY), (endX, endY)) = bounds b

showBoard b = 
    putStrLn $ unlines $ map (unwords . map (show . (b !))) indices 
        where 
            indices = [[(x, y) | x <- [startX..endX]] | y <- [startY..endY]] 
            ((startX, startY), (endX, endY)) = bounds b
            
possibleMoves b (x, y) = 
    [(x-1, y-2),
    (x+1, y-2),
    (x+2, y-1),
    (x+2, y+1),
    (x+1, y+2),
    (x-1, y+2),
    (x-2, y+1),
    (x-2, y-1)]            

numberOfNotVisited board (x, y) =
    foldl (+) 0 [board ! (a, b) | (a, b) <- possibleMoves board (x, y)]
    
mySort ((a1, b1), n1) ((a2, b2), n2)
    | n1 > n2 = GT
    | n1 < n2 = LT
    | n1 == n2 = EQ
   
warnsdorff board (x, y) = sortBy mySort [((a, b), numberOfNotVisited board (a, b)) | (a, b) <- possibleMoves board (x, y), 
                                                                                               inBoard board (a, b), 
                                                                                               board ! (a, b) == 1]

tour b p res
    | warnsdorff b p == [] = res
   -- | (snd $ head $ warnsdorff b p) == 0 = res
    | True = do
        let p' = fst $ head $ warnsdorff b p
        let b' = addKnight b p'
        tour b' p' (res ++ [p'])
        
main = do
    n <- getLine
    let b = addKnight (emptyBoard $ read n) (1, 1)
    print $ length $ tour b (1, 1) [(1, 1)]