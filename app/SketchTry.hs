{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module SketchTry (obj, run) where

import Data.Function ((&))
import Debug.Trace
import OpenSCAD
import Sketch

obj :: OpenSCADM Model3d
obj = do
  let (Right r) = sketch do
        a <- point & x 0 & y 0
        b <- point & x 4 -- y is not set, but solved with constraints
        v1 <- line & from a & degree 0
        onLine b v1
        v2 <- line & from a & degree 30
        v3 <- line & from b & degree 90
        c <- intersectionPoint v2 v3
        poly [a, b, c]
  pure $ linearExtrudeDefault 1 r

run :: IO ()
run =
  render obj & writeFile "SketchTry.scad"
