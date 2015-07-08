{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Types
import Render
import Mechanics
import Data.Char
import System.Environment
import Parser
import Debug.Trace as D

add (x,y) (dx,dy) = (x+dx,y+dy)

acceleration =
    let stg = [(x,y) | y<-[-1,0,1], x<-[-1,0,1]]
    in \input -> stg !! (input - 1)
    
gameLoop :: Track -> Trace -> CarState -> IO ()
gameLoop track trace car@(start, velocity)  = do
  let potentialPositions = map (\i -> add start $ add velocity (acceleration i)) [1..9]
  raceToSvg "file.svg" track (start, potentialPositions)  [ trace ]
  input :: Int <- readLn
  let newvelocity = D.trace ("input " ++ show input)
                    add velocity (acceleration input)
  let newposition = D.trace ("newvelocity " ++ show newvelocity)
                    add start newvelocity
  if crashes car newposition track then
      putStrLn "You crashed !!!"
  else if completes car newposition track then
           putStrLn (show $ length trace)
       else
           gameLoop track (newposition:trace) (newposition, newvelocity)
 

main :: IO ()
main = do
  args <- getArgs
  track <- readFile (head args) >>= return . readTrack
  gameLoop track [startingPoint track] (startingPoint track, (0,0))
