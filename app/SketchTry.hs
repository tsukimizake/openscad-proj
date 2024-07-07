module SketchTry (obj, run) where

import Data.Function ((&))
import Debug.Trace
import OpenSCAD
import Sketch
import SketchTypes

obj :: OpenSCADM Model3d
obj = do
  let ~(Right r) = traceShowId $ sketch
        do
          a <- point & x 0 & y 0
          b <- point & x 4
          c <- point
          d <- point & y 4
          putEq a.y b.y
          putEq a.x d.x
          putEq c.x b.x
          putEq c.y d.y
          poly [a, b, c, d]
  pure $ linearExtrudeDefault 1 r

run :: IO ()
run =
  render obj & writeFile "SketchTry.scad"
