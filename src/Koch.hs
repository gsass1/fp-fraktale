module Koch(newKochFractal) where

import Fractal

import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL

data Point = Point Float Float

data Line = Line
  { begin :: Point
  , end :: Point
  }

transform :: Line -> [Line]
transform (Line (Point x1 y1) (Point x2 y2))
  = [ Line (Point x1 y1) p1
    , Line p1 mid'
    , Line mid' p3
    , Line p3 (Point x2 y2)
    ]
    where dx = x2 - x1
          dy = y2 - y1
          angle = atan2 dy dx
          len = sqrt (dx^2 + dy^2)
          halfLen = len * 0.5
          thirdLen = len * 0.3333
          p1 = Point (x1 + cos (angle) * thirdLen) (y1 + sin (angle) * thirdLen)
          p3 = Point (x1 + cos (angle) * thirdLen * 2) (y1 + sin (angle) * thirdLen * 2)
          normalX = ((-dy)/len)
          normalY = (dx/len)
          midX = (x1 + cos (angle) * halfLen)
          midY = (y1 + sin (angle) * halfLen)
          mid' = Point (midX + normalX * thirdLen) (midY + normalY * thirdLen)

initialState :: [Line]
initialState
  = [ Line (Point (0.5) (-0.5)) (Point (-0.5)  (-0.5))
    , Line (Point (-0.5) (-0.5)) (Point 0.0  0.5)
    , Line (Point 0.0 0.5) (Point 0.5 (-0.5))
    ]

vertices :: Line -> [Float]
vertices (Line (Point x1 y1) (Point x2 y2))
  = [ x1, y1, 0.0
    , x2, y2, 0.0
    ]

-- | Generates the fractal
kochFractal :: Int -> [Line]
kochFractal = (iterate (concatMap transform) initialState !!)

kochVertices :: Int -> V.Vector Float
kochVertices n = V.fromList (concatMap vertices (kochFractal n))

newKochFractal = Fractal GL.Lines kochVertices 
