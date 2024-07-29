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
    lb :: Polygon,
    rb :: Polygon,
    lt :: Polygon,
    rt :: Polygon,
    top :: Polygon,
    bottom :: Polygon,
    left :: Polygon,
    right :: Polygon,
    holder :: Polygon,
    centerHole :: Polygon,
    screwLa :: Point,
    screwLb :: Point,
    screwLc :: Point,
    screwLd :: Point,
    screwRa :: Point,
    screwRb :: Point,
    screwRc :: Point,
    screwRd :: Point
  }

mkSketchRes ''Z

data Y = Y
  { sideframe :: Polygon,
    sockety :: Polygon
  }

mkSketchRes ''Y

data X = X
  {socketx :: Polygon}

mkSketchRes ''X

run :: IO ()
run = obj & render & writeFile "IveFrontCaster2.scad"

mkSocketY :: Point -> SketchM Polygon
mkSocketY center = do
  socketa <- point & relx center (-6.5) & rely center 0
  socketb <- point & relx center 6.5 & rely socketa 0
  socketc <- point & relx center 7.5 & rely socketb 20
  socketd <- point & relx center (-7.5) & rely socketc 0
  poly [socketa, socketb, socketc, socketd]

mkSocketX :: Point -> SketchM Polygon
mkSocketX center = do
  socketa <- point & relx center (-21) & rely center (-17.5)
  socketb <- point & relx center 10 & rely socketa (-1)
  socketc <- point & relx center 10 & rely center 17.5
  socketd <- point & relx center (-21) & rely socketc (-1)
  poly [socketa, socketb, socketc, socketd]

obj :: OpenSCADM Model3d
obj =
  do
    let zrec = sketchRecord do
          -- frame
          a <- point & x 0 & y 0
          c <- point & x 180 & y 48
          frameBottom <- line & from a & degree 0
          frameTop <- line & from c & degree 0
          frameLeft <- line & from a & degree 90
          frameRight <- line & from c & degree 90
          b <- intersectionPoint frameBottom frameRight
          d <- intersectionPoint frameTop frameLeft
          frame <- poly [a, b, c, d]
          ac <- line & between a c
          bd <- line & between b d
          center <- intersectionPoint ac bd

          -- screw holes
          screwLa <- point & relx a 6 & rely center (-18)
          screwLc <- point & relx screwLa 15 & rely center 18
          (screwLb, screwLd) <- rectSketch screwLa screwLc

          screwRa <- point & relx c (-6) & rely center (-18)
          screwRc <- point & relx screwRa (-15) & rely center 18
          (screwRb, screwRd) <- rectSketch screwRa screwRc

          -- reinforce frame
          framel <- point & relx a 0 & rely center 0
          framer <- point & relx c 0 & rely center 0
          frameb <- point & relx center 0 & rely a 0
          framet <- point & relx center 0 & rely c 0
          lb <- wideLine 1.5 framel frameb
          rb <- wideLine 1.5 framer framet
          lt <- wideLine 1.5 framel framet
          rt <- wideLine 1.5 frameb framer
          top <- wideLine 2 c d
          bottom <- wideLine 2 a b
          left <- wideLine 2 a d
          right <- wideLine 2 b c

          -- holder
          holdera <- point & relx center (-12.5) & rely center (-24)
          holderc <- point & relx holdera 25 & rely holdera 48
          (holderb, holderd) <- rectSketch holdera holderc
          holder <- poly [holdera, holderb, holderc, holderd]

          -- center hole
          centerHolea <- point & relx center (-6.5) & rely center (-6.5)
          centerHolec <- point & relx center 6.5 & rely center 6.5
          (centerHoleb, centerHoled) <- rectSketch centerHolea centerHolec
          centerHole <- poly [centerHolea, centerHoleb, centerHolec, centerHoled]
          pure Z {..}

    let yrec = sketchRecord do
          a <- point & x 0 & y 0
          aup <- point & x 0 & y 5
          b <- point & x 80 & y 20
          c <- point & x 100 & y 20
          dup <- point & x 180 & y 5
          d <- point & x 180 & y 0
          sideframe <- poly [a, aup, b, c, dup, d]

          -- socket
          center <- point & x 90 & y 5
          sockety <- mkSocketY center
          pure Y {..}

    let xrec = sketchRecord do
          center <- point & x 24 & y 24
          socketx <- mkSocketX center
          pure X {..}
    let reinforceFrame = union [zrec.lb, zrec.rb, zrec.lt, zrec.rt, zrec.top, zrec.bottom, zrec.left, zrec.right]

    linearExtrudeDefault 2 zrec.frame
      & with union (reinforceFrame & linearExtrudeDefault 20)
      & with intersection (zrec.frame & linearExtrudeDefault 100)
      & with intersection (yrec.sideframe & linearExtrudeDefault 100 & onYAxis)
      & with union (zrec.holder & linearExtrudeDefault 20)
      & diff
        ( intersection
            [ yrec.sockety & sketchExtrude 0 100 OnYAxis,
              xrec.socketx & sketchExtrude 0 200 OnXAxis
            ]
        )
      & diff
        ( union
            [ screwHole M5 35 True & translate (expandVector zrec.screwLa),
              screwHole M5 35 True & translate (expandVector zrec.screwLb),
              screwHole M5 35 True & translate (expandVector zrec.screwLc),
              screwHole M5 35 True & translate (expandVector zrec.screwLd),
              screwHole M5 35 True & translate (expandVector zrec.screwRa),
              screwHole M5 35 True & translate (expandVector zrec.screwRb),
              screwHole M5 35 True & translate (expandVector zrec.screwRc),
              screwHole M5 35 True & translate (expandVector zrec.screwRd)
            ]
        )
      & diff (zrec.centerHole & sketchExtrude (-1) 30 OnZAxis)
      & pure

expandVector :: Vector2d -> Vector3d
expandVector (x_, y_) = (x_, y_, 0)

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
