{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.Either (isRight)
import Data.Function ((&))
import OpenSCAD (Model2d, OpenSCADM, translate, union)
import Sketch
import SketchTypes (SketchError)
import Test.Hspec

rect :: (Either SketchError Model2d)
rect = sketch do
  a <- point & x 0 & y 0
  b <- point & x 4 & y 0
  c <- point & x 4 & y 4
  d <- point & x 0 & y 4
  poly [a, b, c, d]

pitagoras1 :: (Either SketchError Model2d)
pitagoras1 = sketch do
  a <- point & x 0 & y 0
  b <- point & x 4 & y 0
  v1 <- line & from a & degree 30
  v2 <- line & from b & degree 90
  c <- intersection v1 v2
  poly [a, b, c]

pitagoras2 :: (Either SketchError Model2d)
pitagoras2 = sketch do
  a <- point & x 0 & y 0
  b <- point & x 4 -- y is not set, but solved with constraints
  v1 <- line & from a & degree 0
  onLine b v1
  v2 <- line & from a & degree 30
  v3 <- line & from b & degree 90
  c <- intersection v2 v3
  _ <- pure c & x 4 & y 3
  poly [a, b, c]

isoceles :: (Either SketchError Model2d)
isoceles = sketch do
  a <- point & x 0 & y 0
  b <- point & x 4 & y 0
  v1 <- line & from a & degree 40
  v2 <- line & from b & degree 140
  c <- intersection v1 v2
  poly [a, b, c]

example :: OpenSCADM Model2d
example = do
  let ~(Right recte) = rect
  let ~(Right pita1) = pitagoras1
  let ~(Right pita2) = pitagoras2
  let ~(Right iso) = isoceles
  pure $
    union
      [ recte,
        pita1 & OpenSCAD.translate (0, 10),
        OpenSCAD.translate (20, 0) pita2,
        OpenSCAD.translate (30, 0) iso
      ]

main :: IO ()
main = hspec $ do
  describe "SketchExamples" $ do
    it "rect should be Right" $
      rect `shouldSatisfy` isRight

    it "pitagoras1 should be Right" $
      pitagoras1 `shouldSatisfy` isRight

    it "pitagoras2 should be Right" $
      pitagoras2 `shouldSatisfy` isRight

    it "isoceles should be Right" $
      isoceles `shouldSatisfy` isRight
