{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SketchTypes where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer

type SketchM = Eff '[State Id, Writer [Constraint]]

type Angle = Double

type Id = Int

data Sketch
  = P Point
  | LineFunc Line
  | Poly Polygon
  deriving (Show, Eq, Ord)

data Point = Point {x :: Id, y :: Id}
  deriving (Show, Eq, Ord)

instance Shape Point where
  wrapShape = P

data Line = Line {x :: Id, y :: Id, angle :: Id}
  deriving (Show, Eq, Ord)

instance Shape Line where
  wrapShape = LineFunc

newtype Polygon = Polygon [Point]
  deriving (Show, Eq, Ord)

instance Shape Polygon where
  wrapShape = Poly

data SketchError
  = Contradiction String
  | Unresolved String

data Constraint
  = Exact Id Double
  | Eq Id Id
  | OnLine Point Line
  deriving (Show, Eq, Ord)

class Shape a where
  wrapShape :: a -> Sketch
