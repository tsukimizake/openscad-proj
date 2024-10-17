{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module IveRearAdapter3 (obj, run) where

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
    centerCatchr :: Polygon,
    screwHolea :: Point,
    screwHoleb :: Point
  }

mkSketchRes ''Z

data X = X {cut :: Polygon}

mkSketchRes ''X

data Ytilt = Ytilt
  { screwHolea :: Point,
    screwHoleb :: Point,
    topCut :: Polygon,
    bottomCut :: Polygon
  }

mkSketchRes ''Ytilt

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

          totsul <- point & relx centerCatchla 1 & rely c 65
          totsulc <- point & relx totsul 0 & rely c 0
          totsur <- point & relx centerCatchrb (-1) & rely totsul 0
          totsurc <- point & relx totsur 0 & rely c 0
          frame <- poly [a, b, c, totsurc, totsur, totsul, totsulc, d]

          screwHolea <- point & x 20 & y 12
          screwHoleb <- point & relx screwHolea 0 & rely screwHolea 30
          pure Z {..}

    let yres = sketch do
          screwHolea <- point & x 20 & y 25
          screwHoleb <- point & relx screwHolea 0 & rely screwHolea 30
          topCuta <- point & x 0 & rely screwHoleb 5
          topCutb <- point & x 40 & rely topCuta 0
          topCutc <- point & relx topCutb 0 & rely topCuta 15
          topCutd <- point & relx topCuta 0 & rely topCutc 0
          topCut <- poly [topCuta, topCutb, topCutc, topCutd]
          bottomCuta <- point & x 0 & rely screwHolea (-5)
          bottomCutb <- point & x 40 & rely bottomCuta 0
          bottocmcutc <- point & relx bottomCutb 0 & rely bottomCuta (-25)
          bottomCutd <- point & relx bottomCuta 0 & rely bottocmcutc 0
          bottomCut <- poly [bottomCuta, bottomCutb, bottocmcutc, bottomCutd]
          pure Ytilt {..}

    let xres = sketch do
          cuta <- point & x 0 & y 0
          cutb <- point & x 30 & y 0
          v1 <- line & from cutb & degree 35
          cutc <- point & y 45
          _ <- onLine v1 cutc
          cutd <- point & x 0 & y 45
          cut <- poly [cuta, cutb, cutc, cutd]
          pure X {..}

    (zres.frame & sketchExtrude 0 45 OnZAxis)
      & diff (union [zres.innerFrame, zres.lslit, zres.rslit] & sketchExtrude 7.5 (7.5 + 29) OnZAxis)
      & mappend (union [zres.centerCatchl, zres.centerCatchr] & sketchExtrude 0 45 OnZAxis)
      & with intersection (zres.frame & sketchExtrude 0 45 OnZAxis)
      & with intersection (xres.cut & sketchExtrude 0 45 OnXAxis)
      & diff
        ( [yres.screwHolea, yres.screwHoleb]
            & fmap (\hole -> screwHole M5 100 False & rotate3d (-90, 0, 0) & translate (expandVector OnYAxis hole) & rotate3d (-55, 0, 0) & translate (0, 30, 0))
            & union
        )
      & diff
        ( [yres.screwHolea, yres.screwHoleb]
            & fmap (\hole -> screwHole M8 100 False & rotate3d (-90, 0, 0) & translate (expandVector OnYAxis hole) & translate (0, -55, 0) & rotate3d (-55, 0, 0) & translate (0, 30, 0))
            & union
        )
      & diff (union [yres.topCut, yres.bottomCut] & sketchExtrude (-5) 5 OnYAxis & rotate3d (-55, 0, 0) & translate (0, 30, 0))
      & diff ([zres.screwHolea, zres.screwHoleb] & map (\hole -> screwHole M5 10 True & translate (expandVector OnZAxis hole)) & union & translate (0, 0, 45))
      & pure

run :: IO ()
run =
  render obj & writeFile "IveRearAdapter3.scad"
