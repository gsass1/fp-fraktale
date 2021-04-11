module Fractal where

import Shader
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL

data Fractal = Fractal
  { primitiveMode :: GL.PrimitiveMode
  , render :: Int -> V.Vector Float
  }

emptyVertices :: Int -> V.Vector Float
emptyVertices _ = V.fromList []

newEmptyFractal :: Fractal
newEmptyFractal = Fractal GL.Triangles emptyVertices
