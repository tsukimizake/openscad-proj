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
    let zres = sketch do
          (a, b, c, d) <- rectSketch (point & x 0 & y 0) (\_ -> point & x 40 & y 20)

          ac <- line & between a c
          bd <- line & between b d
          center <- intersectionPoint ac bd

          (innera, innerb, innerc, innerd) <- rectSketch (point & relx center (-16) & rely a (-1)) (\_ -> point & relx center 16 & rely c 12)
          innerFrame <- poly [innera, innerb, innerc, innerd]

          (lslita, lslitb, lslitc, lslitd) <- rectSketch (point & relx center (-12.5) & rely center (-20)) (\a_ -> point & relx a_ (-5) & rely c 1)
          lslit <- poly [lslita, lslitb, lslitc, lslitd]

          (rslita, rslitb, rslitc, rslitd) <- rectSketch (point & relx center 12.5 & rely center (-20)) (\a_ -> point & relx a_ 5 & rely c 1)
          rslit <- poly [rslita, rslitb, rslitc, rslitd]

          centerCatchla <- point & relx lslitd 0 & rely innerc 0
          centerCatchlb <- point & relx centerCatchla 0 & rely a 4
          centerCatchlc <- point & relx center (-5) & rely centerCatchlb 0
          centerCatchld <- point & relx center (-4.5) & rely centerCatchla 0
          centerCatchl <- poly [centerCatchla, centerCatchlb, centerCatchlc, centerCatchld]

          centerCatchra <- point & relx rslita 0 & rely innerc 0
          centerCatchrb <- point & relx centerCatchra 0 & rely a 4
          centerCatchrc <- point & relx center 5 & rely centerCatchrb 0
          centerCatchrd <- point & relx center 4.5 & rely centerCatchra 0
          centerCatchr <- poly [centerCatchra, centerCatchrb, centerCatchrc, centerCatchrd]

          totsul <- point & relx centerCatchla 1 & rely c 15
          totsulc <- point & relx totsul 0 & rely c 0
          totsur <- point & relx centerCatchrb (-1) & rely c 15
          totsurc <- point & relx totsur 0 & rely c 0
          frame <- poly [a, b, c, totsurc, totsur, totsul, totsulc, d]
          pure Z {..}

    let yres = sketch do
          (screwHolea, screwHoleb, screwHolec, screwHoled) <-
            rectSketch (point & x (zres.center.x - 7.5) & y 4.5) (\a -> point & relx a 15 & rely a 36)
          pure Y {..}
    (zres.frame & sketchExtrude 0 45 OnZAxis)
      & diff (union [zres.innerFrame, zres.lslit, zres.rslit] & sketchExtrude 7.5 (7.5 + 29) OnZAxis)
      & mappend (union [zres.centerCatchl, zres.centerCatchr] & sketchExtrude 0 45 OnZAxis)
      & diff
        ( [yres.screwHolea, yres.screwHoleb, yres.screwHolec, yres.screwHoled]
            & fmap (\hole -> screwHole M5 20 True & rotate3d (-90, 0, 0) & translate (expandVector OnYAxis hole) & translate (0, 30, 0))
            & union
        )
      & with intersection (zres.frame & sketchExtrude 0 45 OnZAxis)
      & pure

run :: IO ()
run =
  render obj & writeFile "IveRearCaster2.scad"
