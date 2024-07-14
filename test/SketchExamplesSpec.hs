{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.Either (isRight)
import Data.Function ((&))
import Debug.Trace (traceShowM)
import OpenSCAD
import Sketch
import SketchTypes
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
      rect `shouldBe` (Right $ polygon 3 [[(0, 0), (4, 0), (4, 4), (0, 4)]])

    it "eqSpec" do
      let eqSpec = sketch do
            a <- point & x 0 & y 0
            b <- point & x 4
            c <- point & x 8
            d <- point & y 4
            putEq a.y b.y
            putEq a.x d.x
            putEq c.y b.y
            poly [a, b, c, d]
      eqSpec `shouldBe` (Right $ polygon 3 [[(0.0, 0.0), (4.0, 0.0), (8.0, 0.0), (0.0, 4.0)]])

    it "pitagoras1 should be Right" do
      let pitagoras1 = sketch do
            a <- point & x 0 & y 0
            b <- point & x 4 & y 0
            v1 <- line & from a & degree 30
            v2 <- line & from b & degree 90
            c <- intersectionPoint v1 v2
            poly [a, b, c]
      pitagoras1 `shouldBe` (Right $ polygon 3 [[(0.0, 0.0), (4.0, 0.0), (4.0, 1.9999999999999998)]])

    it "pitagoras2 should be Right" do
      let pitagoras2 = sketch do
            a <- point & x 0 & y 0
            b <- point & x 4 -- y is not set, but solved with constraints
            v1 <- line & from a & degree 0
            onLine b v1
            v2 <- line & from a & degree 30
            v3 <- line & from b & degree 90
            c <- intersectionPoint v2 v3
            poly [a, b, c]
      pitagoras2 `shouldBe` (Right $ polygon 3 [[(0.0, 0.0), (4.0, 0.0), (4.0, 1.9999999999999998)]])
    it "pitagoras3 should be Right" do
      let pitagoras3 = sketch do
            a <- point & x 0 -- y is not set, but solved with constraints
            b <- point & x 4 & y 0
            v1 <- line & from a & degree 0
            onLine b v1
            v2 <- line & from a & degree 30
            v3 <- line & from b & degree 90
            c <- intersectionPoint v2 v3
            poly [a, b, c]
      pitagoras3 `shouldBe` (Right $ polygon 3 [[(0.0, 0.0), (4.0, 0.0), (4.0, 1.9999999999999998)]])

    it "isoceles should be Right" do
      let isoceles = sketch do
            a <- point & x 0 & y 0
            b <- point & x 4 & y 0
            v1 <- line & from a & degree 40
            v2 <- line & from b & degree 140
            c <- intersectionPoint v1 v2
            traceShowM c
            poly [a, b, c]
      isoceles `shouldSatisfy` isRight
