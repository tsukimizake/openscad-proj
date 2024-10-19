{-# OPTIONS_GHC -Wno-unresognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module IveRearCaster3 (obj, run) where

import Data.Function ((&))
import OpenSCAD
import Sketch
import SketchTH
import SketchTypes

data Z = Z
  { center :: Point,
    centerhook :: Polygon,
    lhook :: Polygon,
    lhookhead :: Polygon,
    rhook :: Polygon,
    rhookhead :: Polygon,
    casterPillar :: Polygon
  }

mkSketchRes ''Z

data Y = Y
  { screwHolea :: Point,
    screwHoleb :: Point,
    screwHolec :: Point,
    screwHoled :: Point
  }

mkSketchRes ''Y

data X = X
  { adapter :: Polygon
  }

mkSketchRes ''X

obj :: OpenSCADM Model3d
obj =
  do
    let zres = sketch do
          center <- point & x 40 & y 0

          centerhooka <- point & relx center (-4.4) & rely center (-31.5)
          centerhookb <- point & relx center 4.4 & rely center (-31.5)
          centerhookc <- point & relx center 4.9 & rely center 0
          centerhookd <- point & relx center (-4.9) & rely center 0
          centerhook <- poly [centerhooka, centerhookb, centerhookc, centerhookd]

          -- lhook
          (lhooka, lhookb, lhookc, lhookd) <-
            rectSketch
              (point & relx center (-17.5) & rely center (-30.5))
              (\lha -> point & relx lha 3 & rely lha 30.5)
          lhookcinner <- point & relx lhookc 10 & rely lhookc 0
          lhookc2inner <- point & relx lhookc 5 & rely lhookc 0
          lhookc' <- pure lhookc & chamfer 4
          lhook <- poly [lhooka, lhookb, lhookc', lhookcinner, lhookc2inner, lhookd]
          lhookheada <- point & relx lhooka 0 & rely lhooka 0
          lhookheadb <- point & relx lhookheada (-1.5) & rely lhookheada 0 & chamfer 0.5
          lhookheadc <- point & relx lhookheada 0 & rely lhookheada 10
          lhookheadd <- point & relx lhookheadb 0 & rely lhookheadc 0 & chamfer 0.5
          lhookhead <- poly [lhookheada, lhookheadb, lhookheadd, lhookheadc]

          -- rhook
          (rhooka, rhookb, rhookc, rhookd) <-
            rectSketch
              (point & relx center 14.5 & rely center (-30.5))
              (\rha -> point & relx rha 3 & rely rha 30.5)
          rhookdinner <- point & relx rhookd (-10) & rely rhookd 0
          rhookd2inner <- point & relx rhookd (-5) & rely rhookd 0
          rhookd' <- pure rhookd & chamfer 4
          rhook <- poly [rhooka, rhookb, rhookc, rhookdinner, rhookd2inner, rhookd']

          rhookheada <- point & relx rhookb 0 & rely rhookb 0
          rhookheadb <- point & relx rhookheada 1.5 & rely rhookheada 0 & chamfer 0.5
          rhookheadc <- point & relx rhookheada 0 & rely rhookheada 10
          rhookheadd <- point & relx rhookheadb 0 & rely rhookheadc 0 & chamfer 0.5
          rhookhead <- poly [rhookheada, rhookheadb, rhookheadd, rhookheadc]

          casterPillara <- point & relx lhooka 0 & rely lhookc 0
          casterPillarb <- point & relx rhookb 0 & rely casterPillara 0
          casterPillarc <- point & relx casterPillarb 0 & rely casterPillara 20
          casterPillard <- point & relx casterPillara 0 & rely casterPillarc 0
          casterPillar <- poly [casterPillara, casterPillarb, casterPillarc, casterPillard]

          pure Z {..}

    let triangleheight = 5

    let pillarzbottom = -30
    let pillarztop = 17
    let yres = sketch do
          (screwHolea, screwHoleb, screwHolec, screwHoled) <-
            rectSketch (point & x (zres.center.x - 7.5) & y (((pillarzbottom + pillarztop) / 2) - 18)) (\a -> point & relx a 15 & rely a 36)
          pure Y {..}

    let xres = sketch do
          adaptera <- point & x 0 & y 5
          adapterb <- point & relx adaptera (-36) & rely adaptera 0
          adapterc <- point & relx adapterb 0 & rely adaptera (-25)
          adapterd <- point & relx adaptera 0 & rely adaptera (-28)
          adapter <- poly [adaptera, adapterb, adapterc, adapterd]
          pure X {..}
    zres.centerhook
      & sketchExtrude (-23) triangleheight OnZAxis
      & mappend
        ( (union [zres.rhook, zres.rhookhead, zres.lhook, zres.lhookhead] & sketchExtrude (-23) 20 OnZAxis)
            & with intersection (xres.adapter & sketchExtrude 0 100 OnXAxis)
        )
      & mappend (zres.casterPillar & sketchExtrude pillarzbottom pillarztop OnZAxis)
      & diff
        ( union
            [ screwHole M5 10 True & rotate3d (-90, 0, 0) & translate (expandVector OnYAxis yres.screwHolea),
              screwHole M5 10 True & rotate3d (-90, 0, 0) & translate (expandVector OnYAxis yres.screwHoleb),
              screwHole M5 10 True & rotate3d (-90, 0, 0) & translate (expandVector OnYAxis yres.screwHolec),
              screwHole M5 10 True & rotate3d (-90, 0, 0) & translate (expandVector OnYAxis yres.screwHoled)
            ]
            & translate (0, 21, 0)
        )
      & pure

run :: IO ()
run =
  render obj & writeFile "IveRearCaster3.scad"
