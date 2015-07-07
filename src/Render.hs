{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecordWildCards #-}
module Render where

import SVG
import Types
import Parser
    
raceToSvg :: FilePath -> Track -> [ Trace ] -> IO ()
raceToSvg filePath Track{..} traces =
    let svgTraces =
            map (renderLine "green") linesFromTraces
            where
              linesFromTraces = concatMap linesFromTrace traces
              linesFromTrace trace = zip trace (tail trace)
        svgTrack  = map (renderLine "black") boundaries
        svgFinish = renderLine "yellow"  finishLine
    in
      linesToSvg filePath
                 (truncate  xmin,truncate ymin, ceiling xmax,ceiling ymax)
                 (svgTraces ++ svgTrack ++ [svgFinish])
      where
         (xmin,ymin,xmax,ymax) = boundingBox boundaries
         boundingBox lines = foldr1 maxBox $ map dimension lines
         maxBox (xmin1, ymin1, xmax1, ymax1) (xmin2, ymin2, xmax2, ymax2) =
             (min xmin1 xmin2, min ymin1 ymin2, max xmax1 xmax2, max ymax1 ymax2)
