module Main (main) where

import AirClothCap
import HexCup
import HonexCombTest
import IveCaster
import IveFrontAdapter2
import IveFrontCaster2
import IveRearAdapter
import KitchenRack
import LGuard
import PaniaGuard
import Washer

main :: IO ()
main = do
  AirClothCap.run
  HexCup.run
  HonexCombTest.run
  IveCaster.run
  IveFrontAdapter2.run
  IveFrontCaster2.run
  IveRearAdapter.run
  KitchenRack.run
  LGuard.run
  PaniaGuard.run
  Washer.run
