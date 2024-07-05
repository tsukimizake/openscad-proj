{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Sketch (Sketch, wrapShape, sketch, point, x, y, line, from, degree, intersection) where

import Control.Monad.Freer
import Control.Monad.Freer.State (State, get, put, runState)
import Control.Monad.Freer.StateRW
import Control.Monad.Freer.Writer (runWriter)
import Data.Function ((&))
import OpenSCAD (Model2d, OpenSCADM, translate, union)

type Angle = Double

type Id = Int

data Sketch
  = P Point
  | LineFunc Line
  | Poly Polygon
  deriving (Show)

class Shape a where
  wrapShape :: a -> Sketch

data Point = Point {x :: Id, y :: Id}
  deriving (Show)

instance Shape Point where
  wrapShape = P

data Line = Line {x :: Id, y :: Id, angle :: Id}
  deriving (Show)

instance Shape Line where
  wrapShape = LineFunc

newtype Polygon = Polygon [Point]
  deriving (Show)

instance Shape Polygon where
  wrapShape = Poly

type SketchM = Eff '[State Id, Writer [Constraint]]

data Error
  = Contradiction String
  | Unresolved String

--- CONSTRAINTS
data Constraint
  = Exact Id Double
  | Eq Id Id
  | OnLine Point Line
  deriving (Show)

putExact :: Id -> Double -> Eff [State Id, Writer [Constraint]] ()
putExact id v = tell [Exact id v]

putEq :: Id -> Id -> Eff [State Id, Writer [Constraint]] ()
putEq id1 id2 = tell [Eq id1 id2]

--- SOLVER

sketch :: (Shape s) => SketchM s -> OpenSCADM (Either Error Model2d)
sketch m =
  m
    & fmap wrapShape
    & (`runState` 0)
    & fmap fst
    & runWriter
    & run
    & solveConstraints
    & validateAllJust
    & undefined

genId :: SketchM Id
genId = do
  i <- get
  put (i + 1)
  pure i

solveConstraints :: (Sketch, [Constraint]) -> Sketch
solveConstraints = undefined

validateAllJust :: Sketch -> Either Error Sketch
validateAllJust = undefined

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
  angle_ <- genId
  pure $ Line x_ y_ angle_

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
polygon = undefined

--- INTERSECTION

onLine :: Point -> Line -> SketchM ()
onLine p l = do
  tell [OnLine p l]

intersection :: Line -> Line -> SketchM Point
intersection l1 l2 = do
  p <- point
  onLine p l1
  onLine p l2
  pure p

--- EXAMPLE

pitagoras1 :: OpenSCADM (Either Error Model2d)
pitagoras1 = sketch do
  a <- point & x 0 & y 0
  b <- point & x 4 & y 0
  v1 <- line & from a & degree 30
  v2 <- line & from b & degree 90
  c <- intersection v1 v2
  polygon [a, b, c]

pitagoras2 :: OpenSCADM (Either Error Model2d)
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

isoceles :: OpenSCADM (Either Error Model2d)
isoceles = sketch do
  a <- point & x 0 & y 0
  b <- point & x 4 & y 0
  v1 <- line & from a & degree 40
  v2 <- line & from b & degree 140
  c <- intersection v1 v2
  polygon [a, b, c]

obj :: OpenSCADM Model2d
obj = do
  ~(Right pita1) <- pitagoras1
  ~(Right pita2) <- pitagoras2
  ~(Right iso) <- isoceles
  pure $
    union
      [ pita1,
        translate (10, 0) pita2,
        translate (20, 0) iso
      ]
