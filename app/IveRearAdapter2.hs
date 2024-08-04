{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module IveRearAdapter2 (obj, run) where

import Data.Function ((&))
import OpenSCAD
import Sketch
import SketchTH
import SketchTypes

data Z = Z
  { outerFrame :: Polygon,
    innerFrame :: Polygon,
    center :: Point
  }

mkSketchRes ''Z

data Y = Y
  { lscrewHole :: Point,
    rscrewHole :: Point
  }

mkSketchRes ''Y

obj :: OpenSCADM Model3d
obj = do
  let zrec = sketchRecord do
        a <- point & x 0 & y 0
        b <- point & relx a 60 & rely a 0
        c <- point & relx a 30 & rely b 80 & chamfer 5
        outerFrame <- poly [a, b, c]

        innera <- point & relx a 5 & rely a 4
        innerb <- point & relx b (-5) & rely b 4
        innerc <- point & relx c 0 & rely c (-10)
        innerFrame <- poly =<< traverse (chamfer 4 . pure) [innera, innerb, innerc]
        center <- point & relx c 0 & rely a 0

        pure Z {..}

  let yrec = sketchRecord do
        let triangleheight = 4
        zcenter <- point & x zrec.center.x & y 0
        lscrewHole <- point & relx zcenter (-50) & y (triangleheight / 2)
        rscrewHole <- point & relx zcenter 50 & y (triangleheight / 2)
        pure Y {..}

  (zrec.outerFrame & sketchExtrude 0 5 OnZAxis)
    & diff (zrec.innerFrame & sketchExtrude (-1) 6 OnZAxis)
    & diff
      ( union
          [ screwHole M5 10 True & translate (expandVector OnYAxis yrec.lscrewHole),
            screwHole M5 10 True & translate (expandVector OnYAxis yrec.rscrewHole)
          ]
      )
    & pure

run :: IO ()
run =
  render obj & writeFile "IveRearAdapter2.scad"
