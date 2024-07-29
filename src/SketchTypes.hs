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

instance (Models a) => Models (a, a, a, a) where
  type Res (a, a, a, a) = (Res a, Res a, Res a, Res a)
  toList (a, b, c, d) = toList a <> toList b <> toList c <> toList d <&> fmap (const Proxy)
  fromList [(a, pa), (b, pb), (c, pc), (d, pd)] = (fromList [(a, unsafeCoerce pa :: Proxy a)], fromList [(b, unsafeCoerce pb :: Proxy a)], fromList [(c, unsafeCoerce pc :: Proxy a)], fromList [(d, unsafeCoerce pd :: Proxy a)])
  fromList _ = error "fromList: (a, b, c, d)"

instance (Models a) => Models (a, a, a, a, a) where
  type Res (a, a, a, a, a) = (Res a, Res a, Res a, Res a, Res a)
  toList (a, b, c, d, e) = toList a <> toList b <> toList c <> toList d <> toList e <&> fmap (const Proxy)
  fromList [(a, pa), (b, pb), (c, pc), (d, pd), (e, pe)] = (fromList [(a, unsafeCoerce pa :: Proxy a)], fromList [(b, unsafeCoerce pb :: Proxy a)], fromList [(c, unsafeCoerce pc :: Proxy a)], fromList [(d, unsafeCoerce pd :: Proxy a)], fromList [(e, unsafeCoerce pe :: Proxy a)])
  fromList _ = error "fromList: (a, b, c, d, e)"

instance (Models a) => Models (a, a, a, a, a, a) where
  type Res (a, a, a, a, a, a) = (Res a, Res a, Res a, Res a, Res a, Res a)
  toList (a, b, c, d, e, f) = toList a <> toList b <> toList c <> toList d <> toList e <> toList f <&> fmap (const Proxy)
  fromList [(a, pa), (b, pb), (c, pc), (d, pd), (e, pe), (f, pf)] = (fromList [(a, unsafeCoerce pa :: Proxy a)], fromList [(b, unsafeCoerce pb :: Proxy a)], fromList [(c, unsafeCoerce pc :: Proxy a)], fromList [(d, unsafeCoerce pd :: Proxy a)], fromList [(e, unsafeCoerce pe :: Proxy a)], fromList [(f, unsafeCoerce pf :: Proxy a)])
  fromList _ = error "fromList: (a, b, c, d, e, f)"

instance (Models a) => Models (a, a, a, a, a, a, a) where
  type Res (a, a, a, a, a, a, a) = (Res a, Res a, Res a, Res a, Res a, Res a, Res a)
  toList (a, b, c, d, e, f, g) = toList a <> toList b <> toList c <> toList d <> toList e <> toList f <> toList g <&> fmap (const Proxy)
  fromList [(a, pa), (b, pb), (c, pc), (d, pd), (e, pe), (f, pf), (g, pg)] = (fromList [(a, unsafeCoerce pa :: Proxy a)], fromList [(b, unsafeCoerce pb :: Proxy a)], fromList [(c, unsafeCoerce pc :: Proxy a)], fromList [(d, unsafeCoerce pd :: Proxy a)], fromList [(e, unsafeCoerce pe :: Proxy a)], fromList [(f, unsafeCoerce pf :: Proxy a)], fromList [(g, unsafeCoerce pg :: Proxy a)])
  fromList _ = error "fromList: (a, b, c, d, e, f, g)"

instance (Models a) => Models (a, a, a, a, a, a, a, a) where
  type Res (a, a, a, a, a, a, a, a) = (Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a)
  toList (a, b, c, d, e, f, g, h) = toList a <> toList b <> toList c <> toList d <> toList e <> toList f <> toList g <> toList h <&> fmap (const Proxy)
  fromList [(a, pa), (b, pb), (c, pc), (d, pd), (e, pe), (f, pf), (g, pg), (h, ph)] = (fromList [(a, unsafeCoerce pa :: Proxy a)], fromList [(b, unsafeCoerce pb :: Proxy a)], fromList [(c, unsafeCoerce pc :: Proxy a)], fromList [(d, unsafeCoerce pd :: Proxy a)], fromList [(e, unsafeCoerce pe :: Proxy a)], fromList [(f, unsafeCoerce pf :: Proxy a)], fromList [(g, unsafeCoerce pg :: Proxy a)], fromList [(h, unsafeCoerce ph :: Proxy a)])
  fromList _ = error "fromList: (a, b, c, d, e, f, g, h)"

instance (Models a) => Models (a, a, a, a, a, a, a, a, a) where
  type Res (a, a, a, a, a, a, a, a, a) = (Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a)
  toList (a, b, c, d, e, f, g, h, i) = toList a <> toList b <> toList c <> toList d <> toList e <> toList f <> toList g <> toList h <> toList i <&> fmap (const Proxy)
  fromList [(a, pa), (b, pb), (c, pc), (d, pd), (e, pe), (f, pf), (g, pg), (h, ph), (i, pi)] = (fromList [(a, unsafeCoerce pa :: Proxy a)], fromList [(b, unsafeCoerce pb :: Proxy a)], fromList [(c, unsafeCoerce pc :: Proxy a)], fromList [(d, unsafeCoerce pd :: Proxy a)], fromList [(e, unsafeCoerce pe :: Proxy a)], fromList [(f, unsafeCoerce pf :: Proxy a)], fromList [(g, unsafeCoerce pg :: Proxy a)], fromList [(h, unsafeCoerce ph :: Proxy a)], fromList [(i, unsafeCoerce pi :: Proxy a)])
  fromList _ = error "fromList: (a, b, c, d, e, f, g, h, i)"

instance (Models a) => Models (a, a, a, a, a, a, a, a, a, a) where
  type Res (a, a, a, a, a, a, a, a, a, a) = (Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a)
  toList (a, b, c, d, e, f, g, h, i, j) = toList a <> toList b <> toList c <> toList d <> toList e <> toList f <> toList g <> toList h <> toList i <> toList j <&> fmap (const Proxy)
  fromList [(a, pa), (b, pb), (c, pc), (d, pd), (e, pe), (f, pf), (g, pg), (h, ph), (i, pi), (j, pj)] = (fromList [(a, unsafeCoerce pa :: Proxy a)], fromList [(b, unsafeCoerce pb :: Proxy a)], fromList [(c, unsafeCoerce pc :: Proxy a)], fromList [(d, unsafeCoerce pd :: Proxy a)], fromList [(e, unsafeCoerce pe :: Proxy a)], fromList [(f, unsafeCoerce pf :: Proxy a)], fromList [(g, unsafeCoerce pg :: Proxy a)], fromList [(h, unsafeCoerce ph :: Proxy a)], fromList [(i, unsafeCoerce pi :: Proxy a)], fromList [(j, unsafeCoerce pj :: Proxy a)])
  fromList _ = error "fromList: (a, b, c, d, e, f, g, h, i, j)"

instance (Models a) => Models (a, a, a, a, a, a, a, a, a, a, a) where
  type Res (a, a, a, a, a, a, a, a, a, a, a) = (Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a)
  toList (a, b, c, d, e, f, g, h, i, j, k) = toList a <> toList b <> toList c <> toList d <> toList e <> toList f <> toList g <> toList h <> toList i <> toList j <> toList k <&> fmap (const Proxy)
  fromList [(a, pa), (b, pb), (c, pc), (d, pd), (e, pe), (f, pf), (g, pg), (h, ph), (i, pi), (j, pj), (k, pk)] = (fromList [(a, unsafeCoerce pa :: Proxy a)], fromList [(b, unsafeCoerce pb :: Proxy a)], fromList [(c, unsafeCoerce pc :: Proxy a)], fromList [(d, unsafeCoerce pd :: Proxy a)], fromList [(e, unsafeCoerce pe :: Proxy a)], fromList [(f, unsafeCoerce pf :: Proxy a)], fromList [(g, unsafeCoerce pg :: Proxy a)], fromList [(h, unsafeCoerce ph :: Proxy a)], fromList [(i, unsafeCoerce pi :: Proxy a)], fromList [(j, unsafeCoerce pj :: Proxy a)], fromList [(k, unsafeCoerce pk :: Proxy a)])
  fromList _ = error "fromList: (a, b, c, d, e, f, g, h, i, j, k)"

instance (Models a) => Models (a, a, a, a, a, a, a, a, a, a, a, a) where
  type Res (a, a, a, a, a, a, a, a, a, a, a, a) = (Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a)
  toList (a, b, c, d, e, f, g, h, i, j, k, l) = toList a <> toList b <> toList c <> toList d <> toList e <> toList f <> toList g <> toList h <> toList i <> toList j <> toList k <> toList l <&> fmap (const Proxy)
  fromList [(a, pa), (b, pb), (c, pc), (d, pd), (e, pe), (f, pf), (g, pg), (h, ph), (i, pi), (j, pj), (k, pk), (l, pl)] = (fromList [(a, unsafeCoerce pa :: Proxy a)], fromList [(b, unsafeCoerce pb :: Proxy a)], fromList [(c, unsafeCoerce pc :: Proxy a)], fromList [(d, unsafeCoerce pd :: Proxy a)], fromList [(e, unsafeCoerce pe :: Proxy a)], fromList [(f, unsafeCoerce pf :: Proxy a)], fromList [(g, unsafeCoerce pg :: Proxy a)], fromList [(h, unsafeCoerce ph :: Proxy a)], fromList [(i, unsafeCoerce pi :: Proxy a)], fromList [(j, unsafeCoerce pj :: Proxy a)], fromList [(k, unsafeCoerce pk :: Proxy a)], fromList [(l, unsafeCoerce pl :: Proxy a)])
  fromList _ = error "fromList: (a, b, c, d, e, f, g, h, i, j, k, l)"

instance (Models a) => Models (a, a, a, a, a, a, a, a, a, a, a, a, a) where
  type Res (a, a, a, a, a, a, a, a, a, a, a, a, a) = (Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a, Res a)
  toList (a, b, c, d, e, f, g, h, i, j, k, l, m) = toList a <> toList b <> toList c <> toList d <> toList e <> toList f <> toList g <> toList h <> toList i <> toList j <> toList k <> toList l <> toList m <&> fmap (const Proxy)
  fromList [(a, pa), (b, pb), (c, pc), (d, pd), (e, pe), (f, pf), (g, pg), (h, ph), (i, pi), (j, pj), (k, pk), (l, pl), (m, pm)] = (fromList [(a, unsafeCoerce pa :: Proxy a)], fromList [(b, unsafeCoerce pb :: Proxy a)], fromList [(c, unsafeCoerce pc :: Proxy a)], fromList [(d, unsafeCoerce pd :: Proxy a)], fromList [(e, unsafeCoerce pe :: Proxy a)], fromList [(f, unsafeCoerce pf :: Proxy a)], fromList [(g, unsafeCoerce pg :: Proxy a)], fromList [(h, unsafeCoerce ph :: Proxy a)], fromList [(i, unsafeCoerce pi :: Proxy a)], fromList [(j, unsafeCoerce pj :: Proxy a)], fromList [(k, unsafeCoerce pk :: Proxy a)], fromList [(l, unsafeCoerce pl :: Proxy a)], fromList [(m, unsafeCoerce pm :: Proxy a)])
  fromList _ = error "fromList: (a, b, c, d, e, f, g, h, i, j, k, l, m)"

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
  type PointRes Point = Vector2d
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

instance (Points a) => Points (a, a, a, a, a) where
  type PointRes (a, a, a, a, a) = (PointRes a, PointRes a, PointRes a, PointRes a, PointRes a)
  toPointList (a, b, c, d, e) = toPointList a <> toPointList b <> toPointList c <> toPointList d <> toPointList e <&> fmap (const Proxy)
  fromPointList [(a, pa), (b, pb), (c, pc), (d, pd), (e, pe)] = (fromPointList [(a, unsafeCoerce pa :: Proxy a)], fromPointList [(b, unsafeCoerce pb :: Proxy a)], fromPointList [(c, unsafeCoerce pc :: Proxy a)], fromPointList [(d, unsafeCoerce pd :: Proxy a)], fromPointList [(e, unsafeCoerce pe :: Proxy a)])
  fromPointList _ = error "fromPointList: (a, b, c, d, e)"

instance (Points a) => Points (a, a, a, a, a, a) where
  type PointRes (a, a, a, a, a, a) = (PointRes a, PointRes a, PointRes a, PointRes a, PointRes a, PointRes a)
  toPointList (a, b, c, d, e, f) = toPointList a <> toPointList b <> toPointList c <> toPointList d <> toPointList e <> toPointList f <&> fmap (const Proxy)
  fromPointList [(a, pa), (b, pb), (c, pc), (d, pd), (e, pe), (f, pf)] = (fromPointList [(a, unsafeCoerce pa :: Proxy a)], fromPointList [(b, unsafeCoerce pb :: Proxy a)], fromPointList [(c, unsafeCoerce pc :: Proxy a)], fromPointList [(d, unsafeCoerce pd :: Proxy a)], fromPointList [(e, unsafeCoerce pe :: Proxy a)], fromPointList [(f, unsafeCoerce pf :: Proxy a)])
  fromPointList _ = error "fromPointList: (a, b, c, d, e, f)"

instance (Points a) => Points (a, a, a, a, a, a, a) where
  type PointRes (a, a, a, a, a, a, a) = (PointRes a, PointRes a, PointRes a, PointRes a, PointRes a, PointRes a, PointRes a)
  toPointList (a, b, c, d, e, f, g) = toPointList a <> toPointList b <> toPointList c <> toPointList d <> toPointList e <> toPointList f <> toPointList g <&> fmap (const Proxy)
  fromPointList [(a, pa), (b, pb), (c, pc), (d, pd), (e, pe), (f, pf), (g, pg)] = (fromPointList [(a, unsafeCoerce pa :: Proxy a)], fromPointList [(b, unsafeCoerce pb :: Proxy a)], fromPointList [(c, unsafeCoerce pc :: Proxy a)], fromPointList [(d, unsafeCoerce pd :: Proxy a)], fromPointList [(e, unsafeCoerce pe :: Proxy a)], fromPointList [(f, unsafeCoerce pf :: Proxy a)], fromPointList [(g, unsafeCoerce pg :: Proxy a)])
  fromPointList _ = error "fromPointList: (a, b, c, d, e, f, g)"

instance (Points a) => Points (a, a, a, a, a, a, a, a) where
  type PointRes (a, a, a, a, a, a, a, a) = (PointRes a, PointRes a, PointRes a, PointRes a, PointRes a, PointRes a, PointRes a, PointRes a)
  toPointList (a, b, c, d, e, f, g, h) = toPointList a <> toPointList b <> toPointList c <> toPointList d <> toPointList e <> toPointList f <> toPointList g <> toPointList h <&> fmap (const Proxy)
  fromPointList [(a, pa), (b, pb), (c, pc), (d, pd), (e, pe), (f, pf), (g, pg), (h, ph)] = (fromPointList [(a, unsafeCoerce pa :: Proxy a)], fromPointList [(b, unsafeCoerce pb :: Proxy a)], fromPointList [(c, unsafeCoerce pc :: Proxy a)], fromPointList [(d, unsafeCoerce pd :: Proxy a)], fromPointList [(e, unsafeCoerce pe :: Proxy a)], fromPointList [(f, unsafeCoerce pf :: Proxy a)], fromPointList [(g, unsafeCoerce pg :: Proxy a)], fromPointList [(h, unsafeCoerce ph :: Proxy a)])
  fromPointList _ = error "fromPointList: (a, b, c, d, e, f, g, h)"

instance (Points a) => Points (a, a, a, a, a, a, a, a, a) where
  type PointRes (a, a, a, a, a, a, a, a, a) = (PointRes a, PointRes a, PointRes a, PointRes a, PointRes a, PointRes a, PointRes a, PointRes a, PointRes a)
  toPointList (a, b, c, d, e, f, g, h, i) = toPointList a <> toPointList b <> toPointList c <> toPointList d <> toPointList e <> toPointList f <> toPointList g <> toPointList h <> toPointList i <&> fmap (const Proxy)
  fromPointList [(a, pa), (b, pb), (c, pc), (d, pd), (e, pe), (f, pf), (g, pg), (h, ph), (i, pi)] = (fromPointList [(a, unsafeCoerce pa :: Proxy a)], fromPointList [(b, unsafeCoerce pb :: Proxy a)], fromPointList [(c, unsafeCoerce pc :: Proxy a)], fromPointList [(d, unsafeCoerce pd :: Proxy a)], fromPointList [(e, unsafeCoerce pe :: Proxy a)], fromPointList [(f, unsafeCoerce pf :: Proxy a)], fromPointList [(g, unsafeCoerce pg :: Proxy a)], fromPointList [(h, unsafeCoerce ph :: Proxy a)], fromPointList [(i, unsafeCoerce pi :: Proxy a)])
  fromPointList _ = error "fromPointList: (a, b, c, d, e, f, g, h, i)"

instance (Points a) => Points (a, a, a, a, a, a, a, a, a, a) where
  type PointRes (a, a, a, a, a, a, a, a, a, a) = (PointRes a, PointRes a, PointRes a, PointRes a, PointRes a, PointRes a, PointRes a, PointRes a, PointRes a, PointRes a)
  toPointList (a, b, c, d, e, f, g, h, i, j) = toPointList a <> toPointList b <> toPointList c <> toPointList d <> toPointList e <> toPointList f <> toPointList g <> toPointList h <> toPointList i <> toPointList j <&> fmap (const Proxy)
  fromPointList [(a, pa), (b, pb), (c, pc), (d, pd), (e, pe), (f, pf), (g, pg), (h, ph), (i, pi), (j, pj)] = (fromPointList [(a, unsafeCoerce pa :: Proxy a)], fromPointList [(b, unsafeCoerce pb :: Proxy a)], fromPointList [(c, unsafeCoerce pc :: Proxy a)], fromPointList [(d, unsafeCoerce pd :: Proxy a)], fromPointList [(e, unsafeCoerce pe :: Proxy a)], fromPointList [(f, unsafeCoerce pf :: Proxy a)], fromPointList [(g, unsafeCoerce pg :: Proxy a)], fromPointList [(h, unsafeCoerce ph :: Proxy a)], fromPointList [(i, unsafeCoerce pi :: Proxy a)], fromPointList [(j, unsafeCoerce pj :: Proxy a)])
  fromPointList _ = error "fromPointList: (a, b, c, d, e, f, g, h, i, j)"
