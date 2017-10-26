{-
Calculating running time.
Using the System.CPUTime package from Base.
-}

import System.CPUTime
import Control.Exception    (evaluate)

picoSecond = 1e12 :: Double

main = do 
    start <- getCPUTime
    evaluate (sum [1 .. 1000000])
    end <- getCPUTime
    print $  show (fromIntegral (end - start) /picoSecond) ++ " [Sec]"
