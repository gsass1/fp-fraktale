module Sierpinski(newSierpinskiTriangleFractal) where

import Fractal
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL

data Point = Point {x, y :: Float} deriving (Show)
data Triangle = Triangle {a, b, c :: Point} deriving (Show)

getMiddleCoordinate :: Point -> Point -> Point
getMiddleCoordinate (Point x1 y1) (Point x2 y2) = Point ((x1 + x2) / 2) ((y1 + y2) / 2)

sierpinski :: Triangle -> Int -> [Triangle]
sierpinski (Triangle a b c) n
  | n > 0 = list ++ sierpinski (t1) n' ++ sierpinski (t2) n' ++ sierpinski (t3) n'
  | otherwise = list ++ [Triangle a b c]
  where ab = getMiddleCoordinate a b
        bc = getMiddleCoordinate b c
        ac = getMiddleCoordinate a c
        t1 = Triangle ac bc c
        t2 = Triangle a ab ac
        t3 = Triangle ab b bc
        n' = (n - 1)
        list = []

pointToVertices :: Point -> [Float]
pointToVertices (Point x y)
  = [ x, y, 0.0 ]

vertices :: Triangle -> [Float]
vertices (Triangle a b c)
  = (pointToVertices a) ++ (pointToVertices b) ++ (pointToVertices c)

sierpinskiVertices :: Int -> V.Vector Float
sierpinskiVertices n = V.fromList (concatMap vertices (sierpinski (Triangle (Point (negate 4.5) (negate (3 * sqrt 3 / 2))) (Point 4.5 (negate (3 * sqrt 3 / 2))) (Point 0.0 (3 * sqrt 3))) n))

newSierpinskiTriangleFractal = Fractal GL.Triangles sierpinskiVertices
