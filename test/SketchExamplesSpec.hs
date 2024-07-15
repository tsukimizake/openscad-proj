{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.Either (isRight)
import Data.Function ((&))
import OpenSCAD
import Sketch
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "SketchExamples" $ do
    it "rect" do
      let rect = sketch do
            a <- point & x 0 & y 0
            b <- point & x 4 & y 0
            c <- point & x 4 & y 4
            d <- point & x 0 & y 4
            poly [a, b, c, d]
      rect `shouldBe` Right (polygon 3 [[(0, 0), (4, 0), (4, 4), (0, 4)]])

    it "pitagoras1" do
      let pitagoras1 = sketch do
            a <- point & x 0 & y 0
            b <- point & x 4 & y 0
            v1 <- line & from a & degree 30
            v2 <- line & from b & degree 90
            c <- intersectionPoint v1 v2
            poly [a, b, c]
      pitagoras1 `shouldBe` Right (polygon 3 [[(0.0, 0.0), (4.0, 0.0), (4.0, 1.9999999999999998)]])

    it "pitagoras2" do
      let pitagoras2 = sketch do
            a <- point & x 0 & y 0
            b <- point & x 4 -- y is not set, but solved with constraints
            v1 <- line & from a & degree 0
            onLine b v1
            v2 <- line & from a & degree 30
            v3 <- line & from b & degree 90
            c <- intersectionPoint v2 v3
            poly [a, b, c]
      pitagoras2 `shouldBe` Right (polygon 3 [[(0.0, 0.0), (4.0, 0.0), (4.0, 1.9999999999999998)]])
    it "pitagoras3" do
      let pitagoras3 = sketch do
            a <- point & x 0 -- y is not set, but solved with constraints
            b <- point & x 4 & y 0
            v1 <- line & from a & degree 0
            onLine b v1
            v2 <- line & from a & degree 30
            v3 <- line & from b & degree 90
            c <- intersectionPoint v2 v3
            poly [a, b, c]
      pitagoras3 `shouldBe` Right (polygon 3 [[(0.0, 0.0), (4.0, 0.0), (4.0, 1.9999999999999998)]])

    it "isoceles" do
      let isoceles = sketch do
            a <- point & x 0 & y 0
            b <- point & x 4 & y 0
            v1 <- line & from a & degree 40
            v2 <- line & from b & degree 140
            c <- intersectionPoint v1 v2
            poly [a, b, c]
      isoceles `shouldBe` Right (polygon 3 [[(0.0, 0.0), (4.0, 0.0), (2.0000000000000004, 1.6781992623545603)]])
    it "chamferPita" do
      let chamferPita = sketch do
            a <- point & x 0 & y 0
            b <- point & x 4 & y 0
            v1 <- line & from a & degree 30
            v2 <- line & from b & degree 90
            c <- intersectionPoint v1 v2
            poly =<< traverse (chamfer 0.5 . pure) [a, b, c]
      chamferPita `shouldBe` Right ((polygon 3 [[(0.4472135954999579, 0.22360679774997894), (0.5, 0.0), (3.5, 0.0), (4.0, 0.5), (4.0, 1.4999999999999998), (3.552786404500042, 1.7763932022500208)]]))
