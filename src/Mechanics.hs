{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecordWildCards #-}
module Mechanics where

import Types
--import Data.Foldable

intersects :: Line → Line → Bool
intersects ((x1,y1), (x2,y2)) ((x3,y3), (x4,y4)) =
    isCrossing || isSameLine
    where isSameLine = (denominator == 0) && (numerator1 == 0) && (numerator2 == 0)
          isCrossing = denominator /= 0 && (0 <= u1 && u1 <= 1) &&  (0 <= u2 && u2 <= 1)
          denominator = (y4-y3)*(x2-x1) - (x4-x3)*(y2-y1)
          numerator1  = (x4-x3)*(y1-y3) - (y4-y3)*(x1-x3)
          numerator2  = (x2-x1)*(y1-y3) - (y2-y1)*(x1-x3)
          u1 = numerator1 / denominator
          u2 = numerator2 / denominator

completes :: CarState -> Point -> Track -> Bool
completes (start,_) end Track{..} =
     intersects (start,end) finishLine
             
crashes :: CarState -> Point -> Track -> Bool
crashes (start,_) end Track{..} =
    any (intersects (start,end)) boundaries 
