{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
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
      <&> translate (42.1, -13.5, 46.8)
      >>= declModule

  caster <-
    IveFrontCaster2.obj
      >>= declModule
  let inter = intersection [adapter, caster]
  pure $
    union
      [ inter & translucent 0.5,
        adapter & translucent 0.3,
        caster & translucent 0.3
      ]

translucent :: Double -> Model3d -> Model3d
translucent opacity model = model & transparent (Color.withOpacity OpenSCAD.red opacity)

run :: IO ()
run =
  render AsmIveFront.obj & writeFile "AsmIveFront.hs.scad"
