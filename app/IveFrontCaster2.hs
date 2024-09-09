{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module IveFrontCaster2 (obj, run) where

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
run = obj & render & writeFile "IveFrontCaster2.scad"

obj :: OpenSCADM Model3d
obj =
  do
    let zres = sketchRecord do
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

    let yres = sketchRecord do
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
    let xres = sketchRecord do
          center <- point & x 24 & y zres.center.y
          socketInnera <- point & relx center (-7.5) & y socketBottomHeight
          socketInnerb <- point & relx center 7.5 & y socketBottomHeight
          socketInnerc <- point & relx center 8 & y socketTopHeight
          socketInnerd <- point & relx center (-8) & y socketTopHeight
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
            & sketchExtrude socketBottomHeight socketTopHeight OnZAxis
            & with intersection (xres.socketInner & sketchExtrude 0 200 OnXAxis)
        )
      & diff (cylinder 200 3.425 def & rotate3d (0, 90, 0) & translate (expandVector OnXAxis xres.pinHole))
      & pure

-- -- 旧実装
-- obj :: Model3d
-- obj =
--   ( ( linearExtrudeDefault 20 (polygon 3 [[(0, 0), (0, 25), (51, 25), (51, 0)]])
--         & rotate3d (0, -90, 0)
--         & translate (10, 0, 0)
--     )
--       `mappend` (triangle & translate (0, 0, 23))
--       `mappend` (triangle & mirror (1, 0, 0) & translate (0, 0, 23))
--       `mappend` (triangle & rotate3d (0, 7, 0) & translate (0, 0, 23))
--       `mappend` (triangle & rotate3d (0, 7, 0) & mirror (1, 0, 0) & translate (0, 0, 23))
--       `mappend` (triangle & rotate3d (0, -7, 0) & translate (0, 0, 23))
--       `mappend` (triangle & rotate3d (0, -7, 0) & mirror (1, 0, 0) & translate (0, 0, 23))
--       `mappend` (frame & translate (-90, 0, 0))
--       & with minkowski (sphere 0.5 def)
--   )
--     `difference` catcher
--     `difference` pinHole
--     `difference` (screwHoles & translate (83, -4, 8))
--     `difference` (screwHoles & translate (-70, -4, 8))
--
-- frame :: Model3d
-- frame =
--   box 180 3 50
--     `mappend` box 5 4 50
--     `mappend` (box 5 4 50 & translate (180 - 4, 0, 0))
--     `mappend` box 180 4 5
--     `mappend` (box 180 4 5 & translate (0, 0, 50 - 4))
--
-- triangle :: Model3d
-- triangle = polygon 3 [[(0, 0), (0, 22), (90, 0)]] & linearExtrudeDefault 4 & translate (0, 2, 0)
--
-- catcher :: Model3d
-- catcher =
--   rectangle 13 35
--     & linearExtrude 19 0 (0.95, 0.95) 10
--     & rotate3d (90, 0, 0)
--     & translate (-6.5, 26, 7.5)
--
-- pinHole :: Model3d
-- pinHole =
--   cylinder 100 (7 / 2) def & translate (0, 16, -5)
--
-- screwHoles :: Model3d
-- screwHoles =
--   let hole = screwHole M5 15 True & rotate3d (90, 0, 0)
--    in union
--         [ hole & translate (0, 10, 0),
--           hole & translate (15, 10, 0),
--           hole & translate (15, 10, 38),
--           hole & translate (0, 10, 38)
--         ]
--         & translate (-14, 0, -2)
