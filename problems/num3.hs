{-Найти такой путь коня на шахматной доске, чтобы в каждой клетке он побывал только один раз.-}

import Data.Array
import Data.List

emptyBoard n = array ((-1, -1),(n+2, n+2)) 
    [if 1 <= i && i <= n && 1 <= j && j <= n
        then ((i, j), 1) 
        else ((i, j), 0) | i <- [-1..n+2], j <- [-1..n+2]]

addKnight b (x, y) = b // [((x, y), 0)]

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
    
myCompare ((_, _), n1) ((_, _), n2)
    | n1 > n2 = GT
    | n1 < n2 = LT
    | True = EQ

warnsdorff board (x, y) = sortBy myCompare [((a, b), numberOfNotVisited board (a, b)) | (a, b) <- possibleMoves board (x, y), 
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
    print $ tour b (1, 1) [(1, 1)]