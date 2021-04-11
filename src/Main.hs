{-# LANGUAGE OverloadedStrings #-}
-- Konvertiert alle Strings in Text
-- Siehe https://stackoverflow.com/questions/37894987/couldnt-match-expected-type-text-with-actual-type-char

{-# LANGUAGE CPP #-}

module Main where

import Carpet
import EventHandling
import qualified Fractal
import Hilbert
import Koch
import Shader
import State

import System.IO
import System.Exit (exitFailure)

import Data.Char
import Data.Foldable (foldl')
import Data.Text
import Data.Text.IO
import Control.Monad.Loops (iterateUntilM)
import Control.Monad.IO.Class (MonadIO)

import Foreign.C.Types -- CInt
import SDL.Vect -- V2

import Control.Monad -- unless

import Unsafe.Coerce (unsafeCoerce)

import SDL (($=)) -- Importiert $= aus SDL. Erlaubt die Zuweisung mutabeler state variables. Siehe: http://hackage.haskell.org/package/StateVar-1.2/docs/Data-StateVar.html
import qualified SDL -- Importiert das ganze SDL-Packet, mit dem SDL-Prefix

import qualified Graphics.Rendering.OpenGL as GL

import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V

screenWidth, screenHeight :: CInt -- Die Window-Resolution
(screenWidth, screenHeight) = (2560, 1440)

ourGLConfig :: SDL.OpenGLConfig
ourGLConfig = SDL.OpenGLConfig
    { SDL.glColorPrecision = V4 8 8 8 0
    , SDL.glDepthPrecision = 24
    , SDL.glStencilPrecision = 8
    , SDL.glMultisampleSamples = 1
    , SDL.glProfile = SDL.Compatibility SDL.Normal 4 1
    }

mainLoop :: SDL.Window -> Resources -> IO()
mainLoop window resources = do
  let doDraw = draw window

  _ <- iterateUntilM
    exiting
    (\ps ->
      updateProgramState ps <$> SDL.pollEvents
      >>= \ps' -> ps' <$ doDraw ps'
    )
    (initialProgramState resources)

  SDL.destroyWindow window

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo] -- Initialisiert SDL

  window <-
    SDL.createWindow -- Erstellt ein neues Fenster mit unserer Aufloesung
      "FP Projekt: Fraktale"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight,
                         SDL.windowGraphicsContext = SDL.OpenGLContext ourGLConfig}

  renderer <- SDL.glCreateContext window
  resources <- loadResources

  SDL.showWindow window

  mainLoop window resources

  -- Aufraeumen
  SDL.glDeleteContext renderer
  SDL.destroyWindow window
  SDL.quit

loadResources :: IO (Resources)
loadResources = do
  mandelShader <- loadShader "shaders/mandelbrot/vertex.glsl" "shaders/mandelbrot/frag.glsl"
  juliaShader <- loadShader "shaders/julia/vertex.glsl" "shaders/julia/frag.glsl"
  normalShader <- loadShader "shaders/normal/vertex.glsl" "shaders/normal/frag.glsl"
  return (Resources mandelShader juliaShader normalShader)

toGLDouble :: Double -> GL.GLdouble
toGLDouble d = unsafeCoerce d

toGLInt :: Int -> GL.GLint
toGLInt d = unsafeCoerce d

draw :: SDL.Window -> ProgramState -> IO()
draw window ps = case (verts ps) of
                   Just _ -> drawShader window ps (currentFractalShader ps)
                   Nothing -> drawShader window (renderVertices ps) (currentFractalShader ps)

drawShader :: SDL.Window -> ProgramState -> Shader -> IO ()
drawShader window ps (Shader program attrib) = do
    SDL.windowTitle window $= (pack ("Iterations: " ++ (show $ iterations ps)))

    GL.clearColor $= GL.Color4 0 0 0 1
    GL.clear [GL.ColorBuffer]
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral screenWidth) (fromIntegral screenHeight))

    GL.lineWidth $= (2.0 :: GL.GLfloat)
    GL.pointSize $= (realToFrac (zoom ps) :: GL.GLfloat)

    GL.currentProgram $= Just program
    GL.vertexAttribArray attrib $= GL.Enabled
    V.unsafeWith (currentVertices ps) $ \ptr ->
        GL.vertexAttribPointer attrib $=
          (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 ptr)

    let setUniform var value = do
          location <- GL.get (GL.uniformLocation program var)
          GL.uniform location $= value

    setUniform "screen_width" (fromIntegral screenWidth :: GL.GLdouble)
    setUniform "screen_height" (fromIntegral screenHeight :: GL.GLdouble)

    setUniform "cx" (toGLDouble $ posX ps)
    setUniform "cy" (toGLDouble $ posY ps)

    setUniform "zoom" (toGLDouble $ zoom ps)

    setUniform "iterations" (toGLInt $ iterations ps)

    let primitiveMode = currentPrimitiveMode ps

    GL.drawArrays primitiveMode 0 (unsafeCoerce ((V.length (currentVertices  ps)) `div` 3))
    GL.vertexAttribArray attrib $= GL.Disabled

    SDL.glSwapWindow window

screenVertices :: V.Vector Float
screenVertices = V.fromList [-1.0,  1.0,  0.0,
                       -1.0,  -1.0,  0.0,
                       1.0,  -1.0,  0.0,

                       -1.0,  1.0,  0.0,
                       1.0,  -1.0,  0.0,
                      1.0,  1.0,  0.0]

renderFractal :: Int -> Maybe Fractal.Fractal -> V.Vector Float
renderFractal _ Nothing = V.fromList []
renderFractal itr (Just x) = Fractal.render x itr

currentFractalPrimitiveMode :: Maybe Fractal.Fractal -> GL.PrimitiveMode
currentFractalPrimitiveMode Nothing = error "No fractal"
currentFractalPrimitiveMode (Just x) = Fractal.primitiveMode x

currentPrimitiveMode :: ProgramState -> GL.PrimitiveMode
currentPrimitiveMode ps = case (fractal ps) of
                            0 -> GL.Triangles
                            1 -> GL.Triangles
                            _ -> currentFractalPrimitiveMode (fr ps)

currentVertices :: ProgramState -> V.Vector Float
currentVertices ps =
  case (fractal ps) of
    0 -> screenVertices
    1 -> screenVertices
    --_ -> renderFractal ((iterations ps) `div` 10) (fr ps)
    _ -> (maybe (V.fromList []) id (verts ps))
