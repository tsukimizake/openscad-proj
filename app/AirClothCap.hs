{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
module AirClothCap (obj, run) where

import Data.Function ((&))
import Data.Functor ((<&>))
import HoneyCombWall (honeyCombWall)
import OpenSCAD

obj :: OpenSCADM Model3d
obj =
  do
    let len = 106
    let half = len / 2
    let height = half
    wall <- honeyCombWall 10 (len, half, 2) <&> translate (0, 0, -2)
    let wall1 = wall & rotate3d (90, 0, 0) & translate (-half, -half, 0)
    let wall2 = wall & rotate3d (90, 0, 0) & translate (-half, half, 0)
    let wall3 = wall & rotate3d (90, 0, 90) & translate (-half, -half, 0)
    let wall4 = wall & rotate3d (90, 0, 90) & translate (half, -half, 0)
    ceil <- honeyCombWall 10 (len, len, 2) <&> translate (-half, -half, height)
    pure $ union [plate, wall1, wall2, wall3, wall4, ceil]

plate :: Model Vector3d
plate =
  (box 106 106 2 & translate (-53, -53, 0))
    `difference` (cylinder 4 47 def & translate (0, 0, -1))

run :: IO ()
run =
  render obj & writeFile "AirClothCap.scad"
