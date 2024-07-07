{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Sketch (Sketch, wrapShape, sketch, point, x, y, line, from, degree, intersection, example) where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.State (State, get, put, runState)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Data.Function ((&))
import OpenSCAD (Model2d, OpenSCADM, translate, union)
import SketchSolver (runSolver)
import SketchTypes
import Prelude hiding (id)

putExact :: Id -> Double -> Eff [State Id, Writer [Constraint]] ()
putExact id v = tell [Exact id v]

putEq :: Id -> Id -> Eff [State Id, Writer [Constraint]] ()
putEq id1 id2 = tell [Eq id1 id2]

--- SOLVER

sketch :: (Shape s) => SketchM s -> Either SketchError Model2d
sketch m =
  m
    & fmap wrapShape
    & (runState 0)
    & fmap fst
    & runWriter
    & run
    & runSolver

genId :: SketchM Id
genId = do
  i <- get
  put (i + 1)
  pure i

--- POINT

point :: SketchM Point
point = Point <$> genId <*> genId

x :: Double -> SketchM Point -> SketchM Point
x val m = do
  (Point x_ y_) <- m
  putExact x_ val
  pure $ Point x_ y_

y :: Double -> SketchM Point -> SketchM Point
y val m = do
  (Point x_ y_) <- m
  putExact y_ val
  pure $ Point x_ y_

--- LINE

line :: SketchM Line
line = do
  x_ <- genId
  y_ <- genId
  Line x_ y_ <$> genId

from :: Point -> SketchM Line -> SketchM Line
from (Point x_ y_) m = do
  l <- m
  putEq l.x x_
  putEq l.y y_
  pure l

degree :: Angle -> SketchM Line -> SketchM Line
degree val m = do
  l <- m
  putExact l.angle val
  pure l

--- POLYGON
polygon :: [Point] -> SketchM Polygon
polygon = pure . Polygon

--- INTERSECTION

onLine :: Point -> Line -> SketchM ()
onLine p l = tell [OnLine p l]

intersection :: Line -> Line -> SketchM Point
intersection l1 l2 = do
  p <- point
  onLine p l1
  onLine p l2
  pure p

pitagoras1 :: (Either SketchError Model2d)
pitagoras1 = sketch do
  a <- point & x 0 & y 0
  b <- point & x 4 & y 0
  v1 <- line & from a & degree 30
  v2 <- line & from b & degree 90
  c <- intersection v1 v2
  polygon [a, b, c]

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
  polygon [a, b, c]

isoceles :: (Either SketchError Model2d)
isoceles = sketch do
  a <- point & x 0 & y 0
  b <- point & x 4 & y 0
  v1 <- line & from a & degree 40
  v2 <- line & from b & degree 140
  c <- intersection v1 v2
  polygon [a, b, c]

example :: OpenSCADM Model2d
example = do
  let ~(Right pita1) = pitagoras1
  let ~(Right pita2) = pitagoras2
  let ~(Right iso) = isoceles
  pure $
    union
      [ pita1,
        OpenSCAD.translate (10, 0) pita2,
        OpenSCAD.translate (20, 0) iso
      ]
