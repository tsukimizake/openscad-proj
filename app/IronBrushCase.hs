{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module IronBrushCase (obj, run) where

import Data.Function ((&))
import OpenSCAD
import Sketch

obj :: OpenSCADM Model3d
obj =
  do
    let [out, inn] = sketchPolys do
          aout <- point & x 0 & y 0
          bout <- point & relx aout 0 & rely aout 34
          cout <- point & relx bout 40 & rely bout 0
          dout <- point & relx cout 0 & rely cout (-34)
          out_ <- poly [aout, bout, cout, dout]

          ain <- point & relx aout 5 & rely aout 5
          bin <- point & relx ain 30 & rely ain 0
          cin <- point & relx ain 30 & rely ain 23
          din <- point & relx ain 0 & rely ain 23
          inn_ <- poly [ain, bin, cin, din]
          pure [out_, inn_]

    let sideimpl = sketchPoly do
          a <- point & x 0 & y 0
          b <- point & x 0 & y 120
          vtop <- line & from b & degree 20
          d <- point & x 50 & y 0
          dda <- point & x 30 & y 0
          da <- point & x 25 & y 5
          daa <- point & x 21 & y 0
          c <- (intersectionPoint vtop =<< (line & from d & degree 90)) & chamfer 3
          poly [a, b, c, d, dda, da, daa]
    let side = sideimpl & linearExtrudeDefault 120 & onYAxis
    linearExtrudeDefault 150 (difference out inn)
      & with intersection side
      & with union (linearExtrudeDefault 5 out & translate (0, 0, 3))
      & pure

run :: IO ()
run =
  render obj & writeFile "IronBrushCase.scad"
