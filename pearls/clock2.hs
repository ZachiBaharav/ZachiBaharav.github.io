{-
Calculating running time of a program/function.
This program uses the System.Clock package.
-}


{-# LANGUAGE OverloadedStrings #-} 
-- Needed for the fprint formatting below

import Control.Exception    (evaluate)
import Formatting           (fprint, (%))
import Formatting.Clock     (timeSpecs)
import System.Clock         (getTime, Clock(..))
-- clock has 4 data constructors: 
-- Clock(Monotonic,Realtime,ProcessCPUTime, ThreadCPUTime)

main = do 
    start <- getTime Monotonic
    evaluate (sum [1 .. 1000000])
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end
