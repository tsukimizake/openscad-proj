{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module SketchTry (obj, run) where

import Data.Function ((&))
import Debug.Trace
import OpenSCAD
import Sketch

obj :: OpenSCADM Model3d
obj = do
  let ([r, _], []) = sketch do
        a <- point & x 0 & y 0
        b <- point & x 4 & y 0
        v1 <- line & from a & degree 30
        v2 <- line & from b & degree 90
        c <- intersectionPoint v1 v2
        res <- poly =<< traverse (chamfer 0.3 . pure) [a, b, c]
        pure ([res, res], [])
  pure $ linearExtrudeDefault 1 r

run :: IO ()
run =
  render obj & writeFile "SketchTry.scad"
