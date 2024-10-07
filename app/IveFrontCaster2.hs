{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module IveFrontCaster2 (obj, run, octCylinder) where

import Data.Function ((&))
import OpenSCAD as OS
import Sketch
import SketchTH
import SketchTypes
import Prelude

data Z = Z
  { frame :: Polygon,
    center :: Point,
    lb :: Polygon,
    rb :: Polygon,
    lt :: Polygon,
    rt :: Polygon,
    top :: Polygon,
    bottom :: Polygon,
    left :: Polygon,
    right :: Polygon,
    screwLa :: Point,
    screwLb :: Point,
    screwLc :: Point,
    screwLd :: Point,
    screwRa :: Point,
    screwRb :: Point,
    screwRc :: Point,
    screwRd :: Point,
    screwReinforcel :: Polygon,
    screwReinforcer :: Polygon,
    socketOuter :: Polygon,
    socketInner :: Polygon
  }

mkSketchRes ''Z

data Y = Y
  { sideframe :: Polygon,
    center :: Point
  }

mkSketchRes ''Y

data X = X
  { socketInner :: Polygon,
    pinHole :: Point
  }

mkSketchRes ''X

run :: IO ()
run = do
  obj & render & writeFile "IveFrontCaster2.scad"

obj :: OpenSCADM Model3d
obj =
  do
    let zres = sketch do
          -- frame
          (a, b, c, d) <- rectSketch (point & x 0 & y 0) (\_ -> point & x 180 & y 48)
          frame <- poly [a, b, c, d]
          ac <- line & between a c
          bd <- line & between b d
          center <- intersectionPoint ac bd

          -- screw holes
          (screwLa, screwLb, screwLc, screwLd) <-
            rectSketch (point & relx a 6 & rely center (-18)) (\la -> point & relx la 15 & rely center 18)

          (screwRa, screwRb, screwRc, screwRd) <-
            rectSketch (point & relx c (-6) & rely center (-18)) (\ra -> point & relx ra (-15) & rely center 18)

          -- reinforce frame
          framelu <- point & relx a 0 & rely center 6
          frameld <- point & relx a 0 & rely center (-6)
          frameru <- point & relx c 0 & rely center 6
          framerd <- point & relx c 0 & rely center (-6)

          frameb <- point & relx center 0 & rely a 0
          framet <- point & relx center 0 & rely c 0
          lb <- wideLine 1.5 frameld frameb
          rb <- wideLine 1.5 framerd frameb
          lt <- wideLine 1.5 framelu framet
          rt <- wideLine 1.5 frameru framet
          top <- wideLine 2 c d
          bottom <- wideLine 2 a b
          left <- wideLine 2 a d
          right <- wideLine 2 b c

          screwlbl <- point & relx screwLb 4 & rely b 0
          screwlcl <- point & relx screwLc 4 & rely c 0
          screwReinforcel <- poly [a, screwlbl, screwlcl, d]

          screwrbr <- point & relx screwRb (-4) & rely a 0
          screwrcr <- point & relx screwRc (-4) & rely c 0
          screwReinforcer <- poly [screwrbr, b, c, screwrcr]

          (socketOutera, socketOuterb, socketOuterc, socketOuterd) <-
            rectSketch (point & relx center (-24) & rely center (-24)) (\a_ -> point & relx a_ 48 & rely a_ 48)
          socketOuter <- poly [socketOutera, socketOuterb, socketOuterc, socketOuterd]
          (socketInnera, socketInnerb, socketInnerc, socketInnerd) <-
            rectSketch (point & relx center (-19) & rely center (-8)) (\a_ -> point & relx a_ 38 & rely a_ 16)
          socketInner <- poly [socketInnera, socketInnerb, socketInnerc, socketInnerd]
          pure Z {..}

    let yres = sketch do
          a <- point & x 0 & y 0
          aup <- point & x 0 & y 5
          b <- point & x 80 & y 25
          c <- point & x 100 & y 25
          dup <- point & x 180 & y 5
          d <- point & x 180 & y 0
          sideframe <- poly [a, aup, b, c, dup, d]

          ac <- line & between a c
          bd <- line & between b d
          center <- intersectionPoint ac bd

          pure Y {..}

    let socketTopHeight = 25
    let socketBottomHeight = 4
    let xres = sketch do
          center <- point & x 24 & y zres.center.y
          socketInnera <- point & relx center (-7.5) & y socketBottomHeight
          socketInnerb <- point & relx center 7.5 & y socketBottomHeight
          socketInnerc <- point & relx center 8 & y (socketTopHeight + 1)
          socketInnerd <- point & relx center (-8) & y (socketTopHeight + 1)
          socketInner <- poly [socketInnera, socketInnerb, socketInnerc, socketInnerd]

          pinHole <- point & relx center 0 & y (socketTopHeight - 10)
          pure X {..}
    let reinforceFrame = union [zres.lb, zres.rb, zres.lt, zres.rt, zres.top, zres.bottom, zres.left, zres.right]

    linearExtrudeDefault 2 zres.frame
      & with union (reinforceFrame & linearExtrudeDefault 50 & diff (cylinder 200 9 def & rotate3d (0, 90, 0) & translate (expandVector OnXAxis xres.pinHole)))
      & with intersection (zres.frame & linearExtrudeDefault 100)
      & with intersection (yres.sideframe & linearExtrudeDefault 100 & onYAxis)
      -- & mappend
      --   (yres.socket & sketchExtrude 0 50 OnYAxis & with intersection (zres.frame & sketchExtrude 0 100 OnZAxis))
      -- & diff ((union [yres.socketInner, yres.lslit, yres.rslit] & sketchExtrude (-1) 100 OnYAxis) & with intersection (xres.socket & sketchExtrude 0 300 OnXAxis))
      -- & mappend (union [yres.centerCatchl, yres.centerCatchr] & sketchExtrude 0 50 OnYAxis & with intersection (zres.frame & sketchExtrude 0 100 OnZAxis))
      & mappend (union [zres.screwReinforcel, zres.screwReinforcer] & sketchExtrude 0 4 OnZAxis)
      & diff
        ( union
            [ screwHole M5 35 True & translate (expandVector OnZAxis zres.screwLa),
              screwHole M5 35 True & translate (expandVector OnZAxis zres.screwLb),
              screwHole M5 35 True & translate (expandVector OnZAxis zres.screwLc),
              screwHole M5 35 True & translate (expandVector OnZAxis zres.screwLd),
              screwHole M5 35 True & translate (expandVector OnZAxis zres.screwRa),
              screwHole M5 35 True & translate (expandVector OnZAxis zres.screwRb),
              screwHole M5 35 True & translate (expandVector OnZAxis zres.screwRc),
              screwHole M5 35 True & translate (expandVector OnZAxis zres.screwRd)
            ]
        )
      & mappend (zres.socketOuter & sketchExtrude 0 socketTopHeight OnZAxis)
      & diff
        ( zres.socketInner
            & sketchExtrude socketBottomHeight (socketTopHeight + 1) OnZAxis
            & with intersection (xres.socketInner & sketchExtrude 0 200 OnXAxis)
        )
      & diff (octCylinder 6.6 200 & rotate3d (0, 90, 0) & rotate3d (22.5, 0, 0) & translate (expandVector OnXAxis xres.pinHole))
      & pure

octCylinder :: Double -> Double -> Model3d
octCylinder r h =
  let oct :: Model2d
      oct =
        let rect = square r & translate (-(r / 2), -(r / 2))
         in intersection [rect, rect & rotate2d 45]
   in oct & linearExtrudeDefault h
