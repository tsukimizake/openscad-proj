module IveFrontCaster2 (obj, run) where

import Data.Function ((&))
import Debug.Trace
import OpenSCAD as OS
import Sketch
import Prelude

run :: IO ()
run = do
  obj & render & writeFile "IveFrontCaster2.scad"

obj :: OpenSCADM Model3d
obj =
  do
    let ([frame, bddiag, acdiag, top, bottom, left, right, socket, holder], []) = traceShowId $ sketch do
          -- frame
          a <- point & x 0 & y 0
          c <- point & x 180 & y 50
          frameBottom <- line & from a & degree 0
          frameTop <- line & from c & degree 0
          frameLeft <- line & from a & degree 90
          frameRight <- line & from c & degree 90
          b <- intersectionPoint frameBottom frameRight
          d <- intersectionPoint frameTop frameLeft
          frame' <- poly [a, b, c, d]
          bddiag' <- wideLine 3 b d
          acdiag' <- wideLine 3 a c
          top' <- wideLine 3 c d
          bottom' <- wideLine 3 a b
          left' <- wideLine 3 a d
          right' <- wideLine 3 b c
          ac <- line & between a c
          bd <- line & between b d
          center <- intersectionPoint ac bd

          -- socket
          socketa <- point & relx center (-6.5) & rely center (-17.5)
          socketc <- point & relx socketa 13 & rely socketa 35
          (socketb, socketd) <- rectSketch socketa socketc
          socket' <- poly [socketa, socketb, socketc, socketd]

          -- holder
          holdera <- point & relx center (-12.5) & rely center (-24) -- TODO less height
          holderc <- point & relx holdera 25 & rely holdera 48
          (holderb, holderd) <- rectSketch holdera holderc
          holder' <- poly [holdera, holderb, holderc, holderd]
          pure ([frame', bddiag', acdiag', top', bottom', left', right', socket', holder'], [])

    let ([sideframe], []) = sketch do
          a <- point & x 0 & y 0
          aup <- point & x 0 & y 5
          b <- point & x 80 & y 20
          c <- point & x 100 & y 20
          dup <- point & x 180 & y 5
          d <- point & x 180 & y 0
          sideframe' <- poly [a, aup, b, c, dup, d]
          pure ([sideframe'], [])

    let reinforceFrame =
          union
            [ bddiag,
              acdiag,
              top,
              bottom,
              left,
              right
            ]
    (frame & linearExtrudeDefault 1)
      & with union (reinforceFrame & linearExtrudeDefault 20)
      & with intersection (sideframe & linearExtrudeDefault 100 & onYAxis)
      & with union (holder & linearExtrudeDefault 20)
      & flip difference (socket & linearExtrudeDefault 20 & translate (0, 0, 2))
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
