import Data.Char
import Control.Concurrent
import Control.Concurrent.Chan
import System.Random

bounds = (0::Double, 10::Double)
path = "haskelllab2.txt"

random1point = do
    g1 <- newStdGen; g2 <- newStdGen
    return (zip (randomRs bounds g1) (randomRs bounds g2))
    
random3points = do
    g1 <- newStdGen; g2 <- newStdGen; g3 <- newStdGen; g4 <- newStdGen; g5 <- newStdGen; g6 <- newStdGen
    return (zip3 (zip (randomRs bounds g1) (randomRs bounds g2))
                 (zip (randomRs bounds g3) (randomRs bounds g4))
                 (zip (randomRs bounds g5) (randomRs bounds g6)))
                 
sign x1 y1 x2 y2 x3 y3 = (x1 - x3) * (y2 - y3) - (x2 - x3) * (y1 - y3)
                 
isIn x0 y0 x1 y1 x2 y2 x3 y3 = (b1 == b2) && (b2 == b3)
    where
        b1 = sign x0 y0 x1 y1 x2 y2 < 0
        b2 = sign x0 y0 x2 y2 x3 y3 < 0
        b3 = sign x0 y0 x3 y3 x1 y1 < 0
        
repeatNTimes 0 _ = return ()
repeatNTimes n action = do
    action
    repeatNTimes (n-1) action

main = do
    m1 <- newEmptyMVar
    m2 <- newEmptyMVar

    forkIO $ do
        r <- random1point
        putMVar m1 r
        
    forkIO $ do
        r <- random3points
        putMVar m2 r
    
    repeatNTimes 100 (zu m1 m2)
    
    where 
        zu m1 m2 = do
            r1 <- takeMVar m1
            r2 <- takeMVar m2
            
            let (onepoint, threepoints) = (take 1 r1, take 1 r2)
            let (x0, y0) = head onepoint
            let ((x1, y1), (x2, y2), (x3, y3)) = head threepoints
            
            putMVar m1 (tail r1)
            putMVar m2 (tail r2)
            
            appendFile path $ show (x0, y0, x1, y1, x2, y2, x3, y3, isIn x0 y0 x1 y1 x2 y2 x3 y3) ++ "\n"