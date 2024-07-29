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
import OpenSCAD (Model2d, Vector2d)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (pi)

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

data ResultTH
  = ModelResTH Model2d
  | PointResTH Vector2d
  deriving (Show)

resultToResultTH :: Result -> ResultTH
resultToResultTH = \case
  ModelRes m -> ModelResTH m
  PointRes x y -> PointResTH (x, y)

modelAndVecToResult :: ([Model2d], [Vector2d]) -> [ResultTH]
modelAndVecToResult (models, vecs) = fmap ModelResTH models <> fmap PointResTH vecs

resutlToModelAndVec :: [ResultTH] -> ([Model2d], [Vector2d])
resutlToModelAndVec = foldr f ([], [])
  where
    f (ModelResTH m) (ms, ps) = (m : ms, ps)
    f (PointResTH p) (ms, ps) = (ms, p : ps)

unwrapModelResTH :: ResultTH -> Model2d
unwrapModelResTH = \case
  ModelResTH m -> m
  _ -> error "unwrapModelResTH"

unwrapPointResTH :: ResultTH -> Vector2d
unwrapPointResTH = \case
  PointResTH m -> m
  _ -> error "unwrapPointResTH"

class ModelsTH a where
  type ResTH a :: Type
  toListTH :: a -> ([Sketch], Proxy a)
  fromListTH :: ([ResultTH], Proxy a) -> ResTH a
