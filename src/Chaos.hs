module Chaos(newChaosSierpinskiTriangleFractal, newChaosSquareCarpetFractal, newChaosVicsekFractal) where

import Fractal
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL
import System.Random
import System.IO.Unsafe

randomIdxs :: Int -> [Int]
randomIdxs x = unsafePerformIO $ randomRs (0,x - 1) <$> newStdGen

randomlyCycle :: [a] -> [a]
randomlyCycle x = map (\idx -> x!!idx) (randomIdxs (length x))

randomCycleZipped2 :: [a] -> [(a,a)]
randomCycleZipped2 x = makeZip2 $ randomlyCycle x
  where makeZip2 (x:xs) = [(x,head xs)] ++ makeZip2 xs

randomCycleZipped3 :: [a] -> [(a,a,a)]
randomCycleZipped3 x = makeZip3 $ randomlyCycle x
  where makeZip3 (x:xs) = [(x,head xs,xs!!2)] ++ makeZip3 xs

data Point = Point {x, y :: Float} deriving (Show)

equ :: Point -> Point -> Bool
equ (Point x1 y1) (Point x2 y2) = x1 == x2 && y1 == y2

between :: Float -> Point -> Point -> Point
between r (Point x1 y1) (Point x2 y2) = 
    Point
      ((x1 + x2) * r)
      ((y1 + y2) * r)


triangleCorners =
  [
    Point (negate 0.5) 0,
    Point 0 1,
    Point 0.5 0
  ]

squareVertices =
  [
    Point 0 0,
    Point 1.0 0,
    Point 1.0 1.0,
    Point 0.0 1.0
  ]

squareCenterVertices =
  [
    Point 0 0,
    Point 1.0 0,
    Point 1.0 1.0,
    Point 0.0 1.0,
    Point 0.5 0.5
  ]

pentagonVertices =
  [
    Point 0 1.0,
    Point (negate 0.5) 0.666,
    Point (negate 0.333) 0,
    Point 0.333 0,
    Point 0.5 0.666
  ]

-- | ----------------------------
-- | POINT MAPPERS
-- | ----------------------------

-- | Maps a point to a new point whereby the first argument is the previous
-- point and the second argument is a tuple of the last, previous last, and
-- previous previous last vertex chosen.
--
-- The function can return Nothing if the rule choses not to add a new point
-- (this saves a bit of memory)
type PointMapper = Point -> (Point, Point, Point) -> Maybe Point

-- | Returns the middle point if the previous two vertices are not equal
twoThirds :: PointMapper
twoThirds p (v0, _, _) = 
    Just $ between 0.6666 p v0

oneOverPhi :: PointMapper
oneOverPhi p (v0, _, _) = 
    Just (between 0.61803398875 p v0)

-- | Returns the middle point if the previous two vertices are not equal
midIfPrevNotEql :: PointMapper
midIfPrevNotEql p (v0, v1, _) = 
  if (v0 `equ` v1) then
    Nothing
  else
    Just $ between 0.5 p v0

-- | Returns the middle point if the previous two vertices are not equal
midIfDist :: PointMapper
midIfDist p (Point x1 y1, Point x2 y2, _) = 
  if (sqrt ((x1-x2)^2+(y1-y2)^2) > 1.0) then
    Nothing
  else
    Just $ between 0.5 p (Point x1 y1)

-- | Returns the middle point
mid :: PointMapper
mid p (v0, _, _) =
  Just $ between 0.5 p v0

-- | Does one chaos iteration
chaos :: PointMapper -> [Point] -> (Point, Point, Point) -> [Point]
chaos m previous randomCorner = previous ++ nextPoint
  where nextPoint = makeArr (m (last previous) randomCorner)
        makeArr Nothing = []
        makeArr (Just x) = [x]

vertices :: Point -> [Float]
vertices (Point x y) = [x, y, 0.0]

-- | Generates the fractal vertices taking the point mapper, the initial
-- vertices and an iteration count
chaosVertices :: PointMapper -> [Point] -> Int -> V.Vector Float
chaosVertices m c n = V.fromList (concatMap vertices (foldl (chaos m) c (take (n * 50) randomCorners)))
  where randomCorners = randomCycleZipped3 c

-- | Sierpinski Triangle
newChaosSierpinskiTriangleFractal :: Fractal
newChaosSierpinskiTriangleFractal = Fractal GL.Points (chaosVertices mid triangleCorners)

-- | Square carpet
newChaosSquareCarpetFractal :: Fractal
newChaosSquareCarpetFractal = Fractal GL.Points (chaosVertices midIfPrevNotEql squareVertices)

-- | Vicsek
newChaosVicsekFractal :: Fractal
newChaosVicsekFractal = Fractal GL.Points (chaosVertices midIfDist squareVertices)
