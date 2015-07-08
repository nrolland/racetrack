{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Types
import Render
import Mechanics

add (x,y) (dx,dy) = (x+dx,y+dy)

acceleration =
    let stg = [(x,y) | y<-[-1,0,1], x<-[-1,0,1]]
    in \input -> stg !! (input - 1)
    
gameLoop :: Track -> Trace -> CarState -> IO ()
gameLoop track trace car@(start, velocity)  = do
  raceToSvg "file.svg" track [ trace ]
  input :: Int <- readLn
  let newvelocity = add velocity (acceleration input)
  let newposition = add start newvelocity
  if crashes car newposition track then
      putStrLn "You crashed !!!"
  else if completes car newposition track then
           putStrLn (show $ length trace)
       else
           gameLoop track (start:trace) (newposition, newvelocity)
 

main :: IO ()
main = putStrLn "Hello, world!"
