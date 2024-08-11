{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module IveRearCaster2 (obj, run) where

import Data.Function ((&))
import OpenSCAD
import Sketch
import SketchTH
import SketchTypes

data Z = Z
  { frame :: Polygon,
    innerFrame :: Polygon,
    lslit :: Polygon,
    rslit :: Polygon,
    center :: Point,
    centerCatchl :: Polygon,
    centerCatchr :: Polygon
  }

mkSketchRes ''Z

data Y = Y
  { screwHolea :: Point,
    screwHoleb :: Point,
    screwHolec :: Point,
    screwHoled :: Point
  }

mkSketchRes ''Y

obj :: OpenSCADM Model3d
obj =
  do
    let zres = sketchRecord do
          (a, b, c, d) <- rectSketch (point & x 0 & y 0) (\_ -> point & x 38 & y 25)
          frame <- poly [a, b, c, d]
          ac <- line & between a c
          bd <- line & between b d
          center <- intersectionPoint ac bd

          (innera, innerb, innerc, innerd) <- rectSketch (point & relx center (-16) & rely a (-1)) (\_ -> point & relx center 16 & rely c (-4))
          innerFrame <- poly [innera, innerb, innerc, innerd]

          (lslita, lslitb, lslitc, lslitd) <- rectSketch (point & relx center (-11) & rely center (-5)) (\a_ -> point & relx a_ (-5) & rely c 1)
          lslit <- poly [lslita, lslitb, lslitc, lslitd]

          (rslita, rslitb, rslitc, rslitd) <- rectSketch (point & relx center 11 & rely center (-5)) (\a_ -> point & relx a_ 5 & rely c 1)
          rslit <- poly [rslita, rslitb, rslitc, rslitd]

          centerCatchla <- point & relx lslitd 0 & rely innerc 0
          centerCatchlb <- point & relx centerCatchla 0 & rely c (-21)
          centerCatchlc <- point & relx centerCatchlb 4 & rely centerCatchlb 0
          centerCatchld <- point & relx center (-3.5) & rely centerCatchla 0
          centerCatchl <- poly [centerCatchla, centerCatchlb, centerCatchlc, centerCatchld]

          centerCatchra <- point & relx rslita 0 & rely innerc 0
          centerCatchrb <- point & relx centerCatchra 0 & rely c (-21)
          centerCatchrc <- point & relx centerCatchrb (-4) & rely centerCatchrb 0
          centerCatchrd <- point & relx center 3.5 & rely centerCatchra 0
          centerCatchr <- poly [centerCatchra, centerCatchrb, centerCatchrc, centerCatchrd]
          pure Z {..}

    let yres = sketchRecord do
          (screwHolea, screwHoleb, screwHolec, screwHoled) <-
            rectSketch (point & x (zres.center.x - 7.5) & y 4) (\a -> point & relx a 15 & rely a 36)
          pure Y {..}
    (zres.frame & sketchExtrude 0 45 OnZAxis)
      & diff (union [zres.innerFrame, zres.lslit, zres.rslit] & sketchExtrude 7.5 (7.5 + 29) OnZAxis)
      & diff
        ( [yres.screwHolea, yres.screwHoleb, yres.screwHolec, yres.screwHoled]
            & fmap (\hole -> screwHole M5 10 True & rotate3d (-90, 0, 0) & translate (expandVector OnYAxis hole) & translate (0, 21, 0))
            & union
        )
      & mappend (union [zres.centerCatchl, zres.centerCatchr] & sketchExtrude 0 45 OnZAxis)
      & pure

run :: IO ()
run =
  render obj & writeFile "IveRearCaster2.scad"
