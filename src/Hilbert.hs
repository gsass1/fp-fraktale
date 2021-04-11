module Hilbert(newHilbertFractal) where

import Fractal
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL

data Point = Point {x, y :: Float} deriving (Show)

data Line = Line {s, e :: Point} deriving (Show)

data Polygon = Polygon {a, b, c, d :: Point} deriving (Show)

hilbert :: Int -> Float -> Point -> [Polygon]
hilbert o s (Point x y)
  | o <= 1 = [(Polygon a b c d)]
  | otherwise = rotate cw (hilbert o' s' (Point x y)) s' (Point x y) ++
                hilbert o' s' (Point x y') ++
                hilbert o' s' (Point x' y') ++
                rotate ccw (hilbert o' s' (Point x' y)) s' (Point x' y)
    where
      o' = o - 1
      s' = s / 2
      y' = y + s'
      x' = x + s'

      cw = - pi/2
      ccw = pi/2

      offset = s / fromIntegral (4 * o)

      x0 = x + offset
      y0 = y + offset

      x1 = x + offset
      y1 = y + offset + s'

      x2 = x + offset + s'
      y2 = y + offset + s'

      x3 = x + offset + s'
      y3 = y + offset

      a = (Point x0 y0)
      b = (Point x1 y1)
      c = (Point x2 y2)
      d = (Point x3 y3)

rotate :: Float -> [Polygon] -> Float -> Point -> [Polygon]
rotate r (h:t) s p
  | length (h:t) == 1 && r < 0 = [(Polygon a' d' c' b')]
  | length (h:t) == 1 && r > 0 = [(Polygon c' b' a' d')]
  | otherwise =  points2Polygons (reverse (polygons2Points rps))
    where
      a' = (a h)
      b' = (b h)
      c' = (c h)
      d' = (d h)
      rps = map (rotatePolygon s p r) (h:t)

rotatePolygon :: Float -> Point -> Float -> Polygon -> Polygon
rotatePolygon s p a (Polygon (Point x0 y0) (Point x1 y1) (Point x2 y2) (Point x3 y3)) = (Polygon p0' p1' p2' p3')
  where
    x0' = px0 * ca - py0 * sa + xp
    y0' = px0 * sa + py0 * ca + yp

    x1' = px1 * ca - py1 * sa + xp
    y1' = px1 * sa + py1 * ca + yp

    x2' = px2 * ca - py2 * sa + xp
    y2' = px2 * sa + py2 * ca + yp

    x3' = px3 * ca - py3 * sa + xp
    y3' = px3 * sa + py3 * ca + yp

    xp = (x p) + (s / 2)
    yp = (y p) + (s / 2)

    px0 = x0 - xp
    py0 = y0 - yp
    px1 = x1 - xp
    py1 = y1 - yp
    px2 = x2 - xp
    py2 = y2 - yp
    px3 = x3 - xp
    py3 = y3 - yp

    p0' = (Point x0' y0')
    p1' = (Point x1' y1')
    p2' = (Point x2' y2')
    p3' = (Point x3' y3')

    ca = cos a
    sa = sin a

polygons2Points :: [Polygon] -> [Point]
polygons2Points [] = []
polygons2Points (h:t) = [(a h), (b h), (c h), (d h)] ++ polygons2Points t

points2Polygons :: [Point] -> [Polygon]
points2Polygons [] = []
points2Polygons pts = [(Polygon a b c d)] ++ points2Polygons (drop 4 pts)
  where
    a = pts!!0
    b = pts!!1
    c = pts!!2
    d = pts!!3

-- needed for polygon connection
polygons2Lines :: [Polygon] -> [Line]
polygons2Lines [] = []
polygons2Lines (h:t) = [(Line a' b'), (Line b' c'), (Line c' d'), (Line d' a'')] ++ polygons2Lines t
  where
    a' = (a h)
    b' = (b h)
    c' = (c h)
    d' = (d h)
    a'' = if length t >= 1 then (a (t!!0)) else d'

vertices :: Line -> [Float]
vertices (Line s e) = (pointToVertices s) ++ (pointToVertices e)

-- needed if no connections wished
-- vertices :: Polygon -> [Float]
-- vertices (Polygon a b c d) = (pointToVertices a) ++ (pointToVertices b) ++ (pointToVertices b) ++ (pointToVertices c) ++ (pointToVertices c) ++ (pointToVertices d)

pointToVertices :: Point -> [Float]
pointToVertices (Point x y)
  = [ x, y, 0.0 ]

hilbertVertices :: Int -> V.Vector Float
hilbertVertices n = V.fromList (concatMap vertices (polygons2Lines (hilbert n 10.0 (Point (negate 5.0) (negate 5.0)))))
-- needed if no connections wished
-- hilbertVertices n = V.fromList (concatMap vertices (hilbert n 10.0 (Point (negate 5.0) (negate 5.0))))

newHilbertFractal = Fractal GL.Lines hilbertVertices
