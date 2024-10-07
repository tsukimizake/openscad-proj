{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}
module AsmIveFront (obj, run) where

import qualified Data.Colour as Color
import qualified Data.Colour.SRGB as Color
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified IveFrontAdapter2
import qualified IveFrontCaster2
import OpenSCAD
import Sketch
import SketchTH
import SketchTypes

obj :: OpenSCADM Model3d
obj = do
  adapter <-
    IveFrontAdapter2.obj
      <&> withOrigin (50, 0, 0) (rotate3d (0, 190, 0))
      <&> translate (42.075, -13.5, 46.8)
      >>= declModule

  caster <-
    IveFrontCaster2.obj
      >>= declModule
  let inter = intersection [adapter, caster]
  let ivePin' = ivePin & translucent 0.1 OpenSCAD.yellow & rotate3d (0, 90, 0) & translate (65, 24, 15)
  let pinInter = intersection [union [adapter, caster], ivePin']
  pure $
    union
      [ inter & translucent 0.8 OpenSCAD.red,
        pinInter & translucent 0.8 OpenSCAD.red,
        adapter & translucent 0.1 OpenSCAD.blue,
        caster & translucent 0.1 OpenSCAD.green,
        ivePin'
      ]

translucent :: Double -> Color.Colour Double -> Model3d -> Model3d
translucent opacity colour model = model & transparent (Color.withOpacity colour opacity)

ivePin :: Model3d
ivePin =
  union
    [ cylinder 53 (6.3 / 2) def
    ]

run :: IO ()
run =
  render AsmIveFront.obj & writeFile "AsmIveFront.hs.scad"
