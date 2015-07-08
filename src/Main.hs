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
    in \input -> stg !! input
    
gameLoop :: Track -> Trace -> CarState -> IO ()
gameLoop track trace car@(start, velocity)  = do
  raceToSvg "file.svg" track [ trace ]
  input :: Int <- getChar >>= \ch -> return (  (ord ch - 49))
  let newvelocity = D.trace ("input " ++ show input)
                    add velocity (acceleration input)
  let newposition = D.trace ("newvelocity " ++ show newvelocity)
                    add start newvelocity
  if crashes car newposition track then
      putStrLn "You crashed !!!"
  else if completes car newposition track then
           putStrLn (show $ length trace)
       else
           gameLoop track (start:trace) (newposition, newvelocity)
 

main :: IO ()
main = do
  args <- getArgs
  track <- readFile (head args) >>= return . readTrack
  gameLoop track [] (startingPoint track, (0,0))
