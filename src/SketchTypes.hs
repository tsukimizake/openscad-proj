{-# LANGUAGE InstanceSigs #-}

module SketchTypes where

import Control.Monad.Freer
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Reader (Reader)
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Map (Map)
import Data.Proxy
import Data.Tuple
import OpenSCAD (Model2d, Vector2d)
import Unsafe.Coerce (unsafeCoerce)

newtype Id = Id Int
  deriving (Eq, Ord)

newtype UnionFind = UnionFind {parent :: Map Id Id} deriving (Show, Eq)

type SketchM = Eff '[State Int, Writer [Constraint]]

type Angle = Double

instance Show Id where
  show (Id i) = "id" ++ show i

data Sketch
  = P Point
  | LineFunc Line
  | Poly Polygon
  deriving (Show, Eq, Ord)

data Point = Point {x :: Id, y :: Id, chamfer :: Double}
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
  deriving (Show, Eq, Ord)

data Constraint
  = Exact Id Double
  | Eq Id Id
  | OnLine Point Line
  | Plus Id Id Double --  left = right + distance
  | Intersection Line Line Point
  | WideLine Double (Point, Point) (Point, Point, Point, Point)
  deriving (Show, Eq, Ord)

class Shape a where
  wrapShape :: a -> Sketch

type Intersections = [(Line, Line, Point)]

type OnLines = [(Point, Line)]

type Sketches = [Sketch]

type Exacts = [(Id, Double)]

type Eqs = [(Id, Id)]

type Pluses = [(Id, Id, Double)]

type WideLines = [(Double, (Point, Point), (Point, Point, Point, Point))]

type SolverM = Eff '[State (UnionFind, Eqs, Exacts, Pluses), Reader (OnLines, Sketches, Intersections, WideLines), Error SketchError]

data Result
  = ModelRes Model2d
  | PointRes Double Double
  deriving (Show)

class Models a where
  type Res a :: Type
  toList :: a -> [(Sketch, Proxy a)]
  fromList :: [(Result, Proxy a)] -> Res a

instance Models () where
  type Res () = ()
  toList () = []
  fromList [] = ()
  fromList _ = error "fromList: ()"

instance Models Polygon where
  type Res Polygon = Model2d
  toList x = [(wrapShape x, Proxy)]
  fromList [(ModelRes m, _)] = m
  fromList _ = error "fromList: Polygon"

instance (Models a) => Models (a, a) where
  type Res (a, a) = (Res a, Res a)
  toList (a, b) = toList a <> toList b <&> fmap (const Proxy)
  fromList [(a, pa), (b, pb)] = (fromList [(a, unsafeCoerce pa :: Proxy a)], fromList [(b, unsafeCoerce pb :: Proxy a)])
  fromList _ = error "fromList: (a, b)"

instance (Models a) => Models (a, a, a) where
  type Res (a, a, a) = (Res a, Res a, Res a)
  toList (a, b, c) = toList a <> toList b <> toList c <&> fmap (const Proxy)
  fromList [(a, pa), (b, pb), (c, pc)] = (fromList [(a, unsafeCoerce pa :: Proxy a)], fromList [(b, unsafeCoerce pb :: Proxy a)], fromList [(c, unsafeCoerce pc :: Proxy a)])
  fromList _ = error "fromList: (a, b, c)"

class Points a where
  type PointRes a :: Type
  toPointList :: a -> [(Point, Proxy a)]
  fromPointList :: [(Result, Proxy a)] -> PointRes a

instance Points () where
  type PointRes () = ()
  toPointList () = []
  fromPointList [] = ()
  fromPointList _ = error "fromPointList: ()"

instance Points Point where
  type PointRes Point = (Double, Double)
  toPointList x = [(x, Proxy)]
  fromPointList [(PointRes x y, _)] = (x, y)
  fromPointList _ = error "fromPointList: Point"

instance (Points a) => Points (a, a) where
  type PointRes (a, a) = (PointRes a, PointRes a)
  toPointList (a, b) = toPointList a <> toPointList b <&> fmap (const Proxy)
  fromPointList [(a, pa), (b, pb)] = (fromPointList [(a, unsafeCoerce pa :: Proxy a)], fromPointList [(b, unsafeCoerce pb :: Proxy a)])
  fromPointList _ = error "fromPointList: (a, b)"

instance (Points a) => Points (a, a, a) where
  type PointRes (a, a, a) = (PointRes a, PointRes a, PointRes a)
  toPointList (a, b, c) = toPointList a <> toPointList b <> toPointList c <&> fmap (const Proxy)
  fromPointList [(a, pa), (b, pb), (c, pc)] = (fromPointList [(a, unsafeCoerce pa :: Proxy a)], fromPointList [(b, unsafeCoerce pb :: Proxy a)], fromPointList [(c, unsafeCoerce pc :: Proxy a)])
  fromPointList _ = error "fromPointList: (a, b, c)"

instance (Points a) => Points (a, a, a, a) where
  type PointRes (a, a, a, a) = (PointRes a, PointRes a, PointRes a, PointRes a)
  toPointList (a, b, c, d) = toPointList a <> toPointList b <> toPointList c <> toPointList d <&> fmap (const Proxy)
  fromPointList [(a, pa), (b, pb), (c, pc), (d, pd)] = (fromPointList [(a, unsafeCoerce pa :: Proxy a)], fromPointList [(b, unsafeCoerce pb :: Proxy a)], fromPointList [(c, unsafeCoerce pc :: Proxy a)], fromPointList [(d, unsafeCoerce pd :: Proxy a)])
  fromPointList _ = error "fromPointList: (a, b, c, d)"
