{-# OPTIONS_GHC -Wno-unresognised-pragmas #-}

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
    center :: Point,
    bottomFrame :: Polygon,
    centerhook :: Polygon,
    lhook :: Polygon,
    lhookhead :: Polygon,
    rhook :: Polygon,
    rhookhead :: Polygon
  }

mkSketchRes ''Z

data Y = Y
  { lscrewHole :: Point,
    rscrewHole :: Point
  }

mkSketchRes ''Y

data X = X
  { adapter :: Polygon
  }

mkSketchRes ''X

obj :: OpenSCADM Model3d
obj = do
  let zres = sketchRecord do
        a <- point & x 0 & y 0 & chamfer 3
        b <- point & relx a 80 & rely a 0 & chamfer 3
        c <- point & relx a 40 & rely b 90 & chamfer 5
        outerFrame <- poly [a, b, c]

        innera <- point & relx a 7 & rely a 5
        innerb <- point & relx b (-7) & rely b 5
        innerc <- point & relx c 0 & rely c (-10)
        innerFrame <- poly =<< traverse (chamfer 4 . pure) [innera, innerb, innerc]
        center <- point & relx c 0 & rely a 0
        mida <- point & relx a 7 & rely a 2.5
        midb <- point & relx b (-7) & rely b 2.5
        bottomFrame <- wideLine 2.5 mida midb

        centerhooka <- point & relx center (-3) & rely center (-20)
        centerhookb <- point & relx center 3 & rely center (-20)
        centerhookc <- point & relx center 5 & rely center 0
        centerhookd <- point & relx center (-5) & rely center 0
        centerhook <- poly [centerhooka, centerhookb, centerhookc, centerhookd]

        -- lhook
        (lhooka, lhookb, lhookc, lhookd) <-
          rectSketch
            (point & relx center (-20) & rely center (-30))
            (\lha -> point & relx lha 3 & rely lha 30)
        lhookcinner <- point & relx lhookc 10 & rely lhookc 0
        lhookc2inner <- point & relx lhookc 5 & rely lhookc 0
        lhookc' <- pure lhookc & chamfer 5
        lhook <- poly [lhooka, lhookb, lhookc', lhookcinner, lhookc2inner, lhookd]
        lhookheada <- point & relx lhooka 0 & rely lhooka 0
        lhookheadb <- point & relx lhookheada (-1.5) & rely lhookheada 0 & chamfer 0.5
        lhookheadc <- point & relx lhookheada 0 & rely lhookheada 4
        lhookheadd <- point & relx lhookheadb 0 & rely lhookheadb 4 & chamfer 0.5
        lhookhead <- poly [lhookheada, lhookheadb, lhookheadd, lhookheadc]

        -- rhook
        (rhooka, rhookb, rhookc, rhookd) <-
          rectSketch
            (point & relx center 17 & rely center (-30))
            (\rha -> point & relx rha 3 & rely rha 30)
        rhookdinner <- point & relx rhookd (-10) & rely rhookd 0
        rhookd2inner <- point & relx rhookd (-5) & rely rhookd 0
        rhookd' <- pure rhookd & chamfer 5
        rhook <- poly [rhooka, rhookb, rhookc, rhookdinner, rhookd2inner, rhookd']

        rhookheada <- point & relx rhookb 0 & rely rhookb 0
        rhookheadb <- point & relx rhookheada 1.5 & rely rhookheada 0 & chamfer 0.5
        rhookheadc <- point & relx rhookheada 0 & rely rhookheada 4
        rhookheadd <- point & relx rhookheadb 0 & rely rhookheadb 4 & chamfer 0.5
        rhookhead <- poly [rhookheada, rhookheadb, rhookheadd, rhookheadc]

        pure Z {..}

  let triangleheight = 5

  let yres = sketchRecord do
        zcenter <- point & x zres.center.x & y 0
        lscrewHole <- point & relx zcenter (-25) & y (-18)
        rscrewHole <- point & relx zcenter 25 & rely lscrewHole 0
        pure Y {..}

  let xres = sketchRecord do
        adaptera <- point & x 0 & y 5
        adapterb <- point & relx adaptera (-31) & rely adaptera 0
        adapterc <- point & relx adapterb 0 & rely adaptera (-25)
        adapterd <- point & relx adaptera 0 & rely adaptera (-28)
        adapter <- poly [adaptera, adapterb, adapterc, adapterd]
        pure X {..}

  zres.outerFrame
    & sketchExtrude 0 triangleheight OnZAxis
    & mappend (zres.bottomFrame & sketchExtrude (-23) 0 OnZAxis)
    & mappend
      ( (union [zres.rhook, zres.rhookhead, zres.lhook, zres.lhookhead, zres.centerhook] & sketchExtrude (-23) triangleheight OnZAxis)
          & with intersection (xres.adapter & sketchExtrude 0 100 OnXAxis)
      )
    & diff (zres.innerFrame & sketchExtrude (-100) (triangleheight + 1) OnZAxis)
    & diff
      ( union
          [ screwHole M5 10 True & rotate3d (-90, 0, 0) & translate (expandVector OnYAxis yres.lscrewHole),
            screwHole M5 10 True & rotate3d (-90, 0, 0) & translate (expandVector OnYAxis yres.rscrewHole)
          ]
      )
    & pure

run :: IO ()
run =
  render obj & writeFile "IveRearAdapter2.scad"
