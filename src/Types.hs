module Types where


type Point = (Float, Float)
type Line = (Point, Point)
type Velocity = (Float,Float)

type CarState = (Point, Velocity)
type Trace = [Point]

data Track = Track { boundaries :: [Line],
                     startingPoint :: Point,
                     finishLine :: Line}
           deriving (Show)

point  :: Float -> Float -> Point
point x y = (x,y)

line :: Float -> Float -> Float -> Float -> Line
line x1 y1 x2 y2 = ((x1,y1),(x2,y2))
