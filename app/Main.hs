module Main (main) where

import AirClothCap
import HexCup
import HonexCombTest
import IronBrushCase
import IveCaster
import IveFrontAdapter2
import IveFrontCaster2
import IveRearAdapter
import IveRearAdapter2
import KitchenRack
import LGuard
import PaniaGuard
import SketchTH
import SketchTry
import Washer

main :: IO ()
main = do
  IveFrontAdapter2.run
  IveFrontCaster2.run
  IveRearAdapter2.run
