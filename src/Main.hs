{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Types
import Render
import Mechanics
    
gameLoop :: Track -> Trace -> CarState -> IO ()
gameLoop track trace car@(start, velocity)  = do
  raceToSvg "file.svg" track [trace]
  input :: Int  <- readLn 
  let newposition = start
  let newvelocity = velocity
  if crashes car newposition track then
      putStrLn "You crashed !!!"
  else if completes car newposition track then
           putStrLn (show $ length trace)
       else
           gameLoop track (start:trace) (newposition, newvelocity)


main :: IO ()
main = putStrLn "Hello, world!"
