{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecordWildCards #-}
module Render where

import SVG
import Types
    
raceToSvg :: FilePath -> Track -> (Point,[Point]) -> [ Trace ] -> IO ()
raceToSvg filePath Track{..} (carPosition, carPotentialPositions) traces =
    let svgTraces =
            map (renderLine "green") linesFromTraces
            where
              linesFromTraces = concatMap linesFromTrace traces
              linesFromTrace trace = zip trace (tail trace)
        svgTrack  = map (renderLine "black") boundaries
        svgFinish = renderLine "yellow"  finishLine
        svgPotentialMoves =  map (renderLine "blue") $ map (\i -> (carPosition, i)) carPotentialPositions 
    in
      linesToSvg filePath
                 (truncate  xmin,truncate ymin, ceiling xmax,ceiling ymax)
                 (svgTraces ++ svgTrack ++ [svgFinish] ++ svgPotentialMoves)
      where
         (xmin,ymin,xmax,ymax) = boundingBox boundaries
         boundingBox lines = foldr1 maxBox $ map dimension lines
         maxBox (xmin1, ymin1, xmax1, ymax1) (xmin2, ymin2, xmax2, ymax2) =
             (min xmin1 xmin2, min ymin1 ymin2, max xmax1 xmax2, max ymax1 ymax2)
