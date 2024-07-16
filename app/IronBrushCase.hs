{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module IronBrushCase (obj, run) where

import Data.Function ((&))
import OpenSCAD
import Sketch

obj :: OpenSCADM Model3d
obj =
  do
    let ~(Right [out, inn]) = sketchPolys do
          aout <- point & x 0 & y 0
          outv1 <- line & from aout & degree 0
          outv2 <- line & from aout & degree 90
          cout <- point & x 40 & y 34
          outv3 <- line & from cout & degree 0
          outv4 <- line & from cout & degree 90
          bout <- intersectionPoint outv2 outv3
          dout <- intersectionPoint outv1 outv4
          out_ <- poly [aout, bout, cout, dout]

          ain <- point & x 5 & y 5
          -- ain <- point & relx a 5 & rely a 5 みたいにしたいね
          inv1 <- line & from ain & degree 0
          inv2 <- line & from ain & degree 90
          cin <- point & x 35 & y 28
          inv3 <- line & from cin & degree 0
          inv4 <- line & from cin & degree 90
          bin <- intersectionPoint inv2 inv3
          din <- intersectionPoint inv1 inv4
          inn_ <- poly [ain, bin, cin, din]
          pure [out_, inn_]

    let ~(Right sideimpl) = sketchPoly do
          a <- point & x 0 & y 0
          b <- point & x 0 & y 120
          vtop <- line & from b & degree 20
          d <- point & x 50 & y 0
          dda <- point & x 30 & y 0
          da <- point & x 25 & y 5
          daa <- point & x 21 & y 0
          c <- (intersectionPoint vtop =<< (line & from d & degree 90)) & chamfer 3
          poly [a, b, c, d, dda, da, daa]
    let side = sideimpl & linearExtrudeDefault 120 & rotate3d (90, 0, 0) & mirror (0, 1, 0)
    intersection [linearExtrudeDefault 150 $ difference out inn, side] & with union (linearExtrudeDefault 5 out & translate (0, 0, 3)) & pure

run :: IO ()
run =
  render obj & writeFile "IronBrushCase.scad"
