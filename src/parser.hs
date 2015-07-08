module Parser where

import Types
    
readFloat :: String -> Float
readFloat str = case reads str of
	[] -> error "not a floating point number"
	(p,_):_ -> p

readInt :: String -> Int
readInt str = case reads str of
	[] -> error "not an integer"
	(p,_):_ -> p


readTrack :: String -> Track
readTrack str =
    
    let (startingPoint: finishLine: boundaries) = lines str
    in Track (map readLine boundaries)
             (readPoint startingPoint)
             (readLine finishLine)                        
    where readPoint str = let (f:s:_) = map read $ words str in point f s
          readLine str = let (a:b:c:d:_) = map read $ words str in line a b c d
